%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha Logic
%%% Hunting process
%%%

-module(piraha_hunt).

-export([start/3,
         start_link/1,
         cancel/1
        ]).

%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%===================================================================
%% Types
%%===================================================================

-record(state, {uas         :: psip_uas:uas(),
                request     :: ersip_sipmsg:sipmsg(),
                fwd_request :: ersip_sipmsg:sipmsg() | undefined,
                group       :: piraha_group:group(),
                uac         :: psip_uac:uac(),
                to_tag      :: ersip_hdr_fromto:tag(),
                uac_id      :: psip_uac:id() | undefined,
                cancelled = false :: boolean()
               }).
-type state() :: #state{}.

-type start_link_ret() :: {ok, pid()} |
                          {error, {already_started, pid()}} |
                          {error, term()}.

%%===================================================================
%% API
%%===================================================================

-spec start_link(term()) -> start_link_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec start(psip_uas:uas(), ersip_sipmsg:sipmsg(), piraha_group:group()) -> ok.
start(UAS, OrigSipMsg, Group) ->
    Args = [UAS, OrigSipMsg, Group],
    case piraha_hunt_sup:start_child([Args]) of
        {ok, _} ->
            ok;
        {error, _} = Error ->
            psip_log:error("cannot start hunting process: ~p", [Error])
    end.

-spec cancel(ersip_uas:id()) -> ok.
cancel(UASId) ->
    case gproc:lookup_local_name({?MODULE, UASId}) of
        Pid when is_pid(Pid) ->
           gen_server:cast(Pid, cancel);
        undefined ->
            not_found
    end.

%%===================================================================
%% gen_server callbacks
%%===================================================================

-spec init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term()} | ignore.
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: state()) ->
    {reply, Reply :: term(), NewState :: state()} |
    {reply, Reply :: term(), NewState :: state(), timeout() | hibernate | {continue, term()}} |
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), Reply :: term(), NewState :: state()} |
    {stop, Reason :: term(), NewState :: state()}.
-spec handle_cast(Request :: term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: state()}.
-spec handle_info(Info :: timeout | term(), State :: state()) ->
    {noreply, NewState :: state()} |
    {noreply, NewState :: state(), timeout() | hibernate | {continue, term()}} |
    {stop, Reason :: term(), NewState :: state()}.
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: state()) ->
    term().
-spec code_change(OldVsn :: (term() | {down, term()}), State :: state(),
                      Extra :: term()) ->
    {ok, NewState :: state()} | {error, Reason :: term()}.



init([UAS, ReqSipMsg, Group]) ->
    State = #state{uas = UAS,
                   request = ReqSipMsg,
                   group = Group,
                   to_tag = {tag, ersip_id:token(crypto:strong_rand_bytes(8))}
                  },
    UASId = psip_uas:id(UAS),
    gproc:add_local_name({?MODULE, UASId}),
    gen_server:cast(self(), start_hunt),
    {ok, State}.

handle_call(Request, _From, State) ->
    psip_log:error("piraha hunt: unexpected call: ~p", [Request]),
    {reply, {error, {unexpected_call, Request}}, State}.

handle_cast(start_hunt, #state{request = Req} = State) ->
    FwdReq = prepare_fwd_req(Req),
    NewState = State#state{fwd_request = FwdReq},
    psip_log:debug("piraha hunt: forward request template: ~n~s", [ersip_sipmsg:serialize(FwdReq)]),
    gen_server:cast(self(), hunt_next),
    {noreply, NewState};
handle_cast(hunt_next, #state{cancelled = true} = State) ->
    psip_log:info("piraha hunt: stop because cancel", []),
    OutResp = ersip_sipmsg:reply(487, State#state.request),
    psip_uas:response(OutResp, State#state.uas),
    {stop, normal, State};
handle_cast(hunt_next, #state{} = State0) ->
    State = State0#state{to_tag = {tag, ersip_id:token(crypto:strong_rand_bytes(8))}},
    case piraha_group:next(State#state.group) of
        not_found ->
            psip_log:warning("piraha hunt: cannot find next target: give up", []),
            NotFoundResp = ersip_sipmsg:reply(404, State#state.request),
            psip_uas:response(NotFoundResp, State#state.uas),
            {stop, normal, State};
        {ok, {DN, URI, Nexthop}, Group1} ->
            psip_log:info("piraha hunt: diversion to ~p ~p via ~s", [DN, URI, ersip_uri:assemble(Nexthop)]),
            SipMsgToSend = prepare_out_req(DN, URI, State#state.fwd_request),
            psip_log:debug("piraha hunt: out message:~n~s", [ersip_sipmsg:serialize(SipMsgToSend)]),
            Self = self(),
            ResultFun = fun({stop, timeout}) -> gen_server:cast(Self, timeout);
                           ({stop, _}) -> ok;
                           ({message, Resp})    -> gen_server:cast(Self, {response, Resp})
                        end,
            UACId = psip_uac:request(SipMsgToSend, Nexthop, ResultFun),
            {noreply, State#state{group = Group1, uac_id = UACId}}
    end;
handle_cast({response, Resp}, #state{} = State) ->
    case ersip_sipmsg:status(Resp) of
        100 ->
            {noreply, State};
        X when X > 100 andalso X =< 199 ->
            psip_log:info("piraha hunt: passing provisional response: ~p", [X]),
            OutResp = prepare_response(Resp, State#state.request, State#state.to_tag),
            psip_uas:response(OutResp, State#state.uas),
            {noreply, State};
        X when X >= 200 andalso X =< 299 ->
            psip_log:info("piraha hunt: got final response: ~p", [X]),
            OutResp = prepare_response(Resp, State#state.request, State#state.to_tag),
            {ok, DialogId1} = ersip_sipmsg:dialog_id(uas, OutResp),
            {ok, DialogId2} = ersip_sipmsg:dialog_id(uac, Resp),
            psip_b2bua:join_dialogs(DialogId1, DialogId2),
            psip_uas:response(OutResp, State#state.uas),
            {stop, normal, State};
        X when X >= 300 andalso X =< 399 ->
            psip_log:warning("piraha hunt: sorry, redirects (~p) are not supported.", [X]),
            gen_server:cast(self(), hunt_next),
            {noreply, State};
        X when X > 400 ->
            psip_log:warning("piraha hunt: hunting next by response code: ~p", [X]),
            gen_server:cast(self(), hunt_next),
            {noreply, State}
    end;
handle_cast(cancel, #state{cancelled = false, uac_id = undefined} = State) ->
    psip_log:info("piraha hunt: cancel: no active request just set flag", []),
    NewState = State#state{cancelled = true},
    {noreply, NewState};
handle_cast(cancel, #state{cancelled = false, uac_id = UACId} = State) ->
    psip_log:info("piraha hunt: cancel: trying to cancel request with id: ~p", [UACId]),
    NewState = State#state{cancelled = true},
    psip_uac:cancel(NewState#state.uac_id),
    {noreply, NewState};
handle_cast(cancel, #state{cancelled = true} = State) ->
    {noreply, State};
handle_cast(Request, State) ->
    psip_log:error("piraha hunt: unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info(Msg, State) ->
    psip_log:error("piraha hunt: unexpected info: ~p", [Msg]),
    {noreply, State}.

terminate(Reason, #state{}) ->
    case Reason of
        normal ->
            psip_log:debug("piraha hunt: finished", []);
        _ ->
            psip_log:error("piraha hunt: finished with error: ~p", [Reason])
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec prepare_fwd_req(ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
prepare_fwd_req(SipMsg0) ->
    SipMsg1 = remove_route_info(SipMsg0),
    SipMsg2 = filter_allow(SipMsg1),
    %% Remove supported from header because this is not our "supported".
    SipMsg3 = ersip_sipmsg:remove(supported, SipMsg2),
    SipMsg  = new_dialog_information(SipMsg3),
    SipMsg.

-spec remove_route_info(ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
remove_route_info(SipMsg) ->
    ersip_sipmsg:remove_list([<<"via">>, route, record_route], SipMsg).

-spec filter_allow(ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
filter_allow(SipMsg) ->
    case ersip_sipmsg:find(allow, SipMsg) of
        {ok, AllowHdr} ->
            MethodSet = ersip_hdr_allow:to_method_set(AllowHdr),
            NewMethodSet = ersip_method_set:intersection(ersip_method_set:invite_set(), MethodSet),
            NewAllowHdr = ersip_hdr_allow:from_method_set(NewMethodSet),
            ersip_sipmsg:set(allow, NewAllowHdr, SipMsg);
        not_found ->
            SipMsg;
        {error, _} = Error ->
            psip_log:warning("piraha hunt: ignore bad Allow header: ~p", [Error]),
            ersip_sipmsg:remove(<<"allow">>, SipMsg)
    end.

-spec new_dialog_information(ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
new_dialog_information(SipMsg0) ->
    %% Generate new Call-ID:
    CallId = ersip_hdr_callid:make(ersip_id:word(crypto:strong_rand_bytes(12))),
    SipMsg1 = ersip_sipmsg:set(callid, CallId, SipMsg0),
    %% Generate new From tag:
    FromHdr0 = ersip_sipmsg:from(SipMsg1),
    NewTag   = {tag, ersip_id:token(crypto:strong_rand_bytes(6))},
    FromHdr  = ersip_hdr_fromto:set_tag(NewTag, FromHdr0),
    SipMsg2  = ersip_sipmsg:set(from, FromHdr, SipMsg1),
    %% Remove old Contact. New contact is placed after when target is
    %% defined.
    SipMsg3  = ersip_sipmsg:remove(contact, SipMsg2),
    %% Remove old To header
    SipMsg   = ersip_sipmsg:remove(to, SipMsg3),
    SipMsg.


-spec prepare_out_req(ersip_nameaddr:display_name(), ersip_uri:uri(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
prepare_out_req(TargetDN, TargetUri, SipMsg0) ->
    %% Create To:
    ToHdr0  = ersip_hdr_fromto:new(),
    ToHdr1  = ersip_hdr_fromto:set_uri(TargetUri, ToHdr0),
    ToHdr   = ersip_hdr_fromto:set_display_name(TargetDN, ToHdr1),
    SipMsg1 = ersip_sipmsg:set(to, ToHdr, SipMsg0),
    %% Generate Contact:
    URI = psip_udp_port:local_uri(),
    Contact = ersip_hdr_contact:new(URI),
    SipMsg2 = ersip_sipmsg:set(contact, [Contact], SipMsg1),
    %% Setup RURI:
    SipMsg  = ersip_sipmsg:set_ruri(TargetUri, SipMsg2),
    SipMsg.


-spec prepare_response(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg(), ersip_hdr_fromto:tag()) -> ersip_sipmsg:sipmsg().
prepare_response(InResp, Request, ToTag) ->
    Reply = ersip_reply:new(ersip_sipmsg:status(InResp),
                            [{reason, ersip_sipmsg:reason(InResp)},
                             {to_tag, ToTag}]),
    OutResp0 = ersip_sipmsg:reply(Reply, Request),
    FilterHdrs = [ersip_hnames:make_key(<<"route">>),
                  ersip_hnames:make_key(<<"record-route">>),
                  ersip_hnames:make_key(<<"contact">>)
                  | ersip_sipmsg:header_keys(OutResp0)],
    CopyHdrs = ersip_sipmsg:header_keys(InResp) -- FilterHdrs,
    OutResp1 = lists:foldl(fun(Hdr, OutR) ->
                                   ersip_sipmsg:copy(Hdr, InResp, OutR)
                           end,
                           OutResp0,
                           CopyHdrs),

    URI = psip_udp_port:local_uri(),
    Contact = ersip_hdr_contact:new(URI),
    OutResp2 = ersip_sipmsg:set(contact, [Contact], OutResp1),

    Body = ersip_sipmsg:body(InResp),
    ersip_sipmsg:set_body(Body, OutResp2).
