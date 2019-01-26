%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha SIP Stack
%%% SIP Dialog
%%%

-module(psip_dialog).

-behaviour(gen_server).

-export([start_link/1,
         uas_request/1,
         uas_response/2
        ]).

%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export_type([trans/0]).

%%===================================================================
%% Types
%%===================================================================

-record(state, {}).
-type state() :: #state{}.

-type start_link_ret() :: {ok, pid()} |
                          {error, {already_started, pid()}} |
                          {error, term()}.
-type trans() :: {trans, pid()}.

%%===================================================================
%% API
%%===================================================================

-spec start_link(term()) -> start_link_ret().
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec uas_request(ersip_sipmsg:sipmsg()) -> process | {reply, ersip_sipmsg:sipmsg()}.
uas_request(SipMsg) ->
    case ersip_dialog:uas_dialog_id(SipMsg) of
        no_dialog ->
            uas_validate_request(SipMsg);
        {ok, DialogId} ->
            case find_dialog(DialogId) of
                not_found ->
                    psip_log:warning("psip dialog: cannot find dialog ~p", [DialogId]),
                    Resp = ersip_sipmsg:reply(481, SipMsg),
                    {reply, Resp};
                {ok, DialogPid} ->
                    try
                        gen_server:call(DialogPid, {uas_request, SipMsg})
                    catch
                        exit:{noproc, _} ->
                            Resp = ersip_sipmsg:reply(481, SipMsg),
                            {reply, Resp}
                    end
            end
    end.

-spec uas_response(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
uas_response(RespSipMsg, ReqSipMsg) ->
    case ersip_dialog:uas_dialog_id(RespSipMsg) of
        no_dialog -> RespSipMsg;
        {ok, DialogId} ->
            case find_dialog(DialogId) of
                not_found ->
                    uas_maybe_create_dialog(RespSipMsg, ReqSipMsg);
                {ok, DialogPid} ->
                    uas_pass_response(DialogPid, RespSipMsg, ReqSipMsg)
            end
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

init([DialogId]) ->
    gproc:add_local_name(DialogId),
    psip_log:debug("psip dialog: started with id: ~p", [DialogId]),
    State = #state{},
    {ok, State}.

handle_call(Request, _From, State) ->
    psip_log:error("psip dialog: unexpected call: ~p", [Request]),
    {reply, {error, {unexpected_call, Request}}, State}.

handle_cast(Request, State) ->
    psip_log:error("psip dialog: unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info(Msg, State) ->
    psip_log:error("psip dialog: unexpected info: ~p", [Msg]),
    {noreply, State}.

terminate(Reason, #state{}) ->
    case Reason of
        normal ->
            psip_log:debug("psip dialog: finished", []);
        _ ->
            psip_log:error("psip dialog: finished with error: ~p", [Reason])
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec find_dialog(ersip_dialog:id()) -> {ok, pid()} | not_found.
find_dialog(DialogId) ->
    case gproc:lookup_local_name(DialogId) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            not_found
    end.


-spec uas_validate_request(ersip_sipmsg:sipmsg()) -> process | {reply, ersip_sipmsg:sipmsg()}.
uas_validate_request(ReqSipMsg) ->
    case need_create_dialog(ReqSipMsg) of
        false -> process;
        true ->
            case ersip_dialog:uas_verify(ReqSipMsg) of
                ok -> process;
                {reply, _} = Reply -> Reply
            end
    end.

-spec uas_maybe_create_dialog(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
uas_maybe_create_dialog(RespSipMsg, ReqSipMsg) ->
    case ersip_sipmsg:status(RespSipMsg) of
        Status when Status > 100, Status =< 299 ->
            case need_create_dialog(ReqSipMsg) of
                true  -> uas_start_dialog(RespSipMsg, ReqSipMsg);
                false -> RespSipMsg
            end;
        _ ->
            RespSipMsg
    end.


-spec uas_start_dialog(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
uas_start_dialog(RespSipMsg, ReqSipMsg) ->
    Args = [ReqSipMsg, RespSipMsg],
    case hbc_ersip_dialog_sup:start_child([Args]) of
        {ok, DialogPid} ->
            uas_pass_response(DialogPid, RespSipMsg, ReqSipMsg);
        {error, _} = Error ->
            psip_log:error("failed to start dialog ~p", [Error]),
            RespSipMsg
    end.

-spec uas_pass_response(pid(), ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
uas_pass_response(DialogPid, RespSipMsg, ReqSipMsg) ->
    try
        gen_server:call(DialogPid, {uas_pass_response, ReqSipMsg, RespSipMsg})
    catch
        exit:{noproc, _} ->
            hbc_log:warning("dialog ~p is finished, pass response without dialog processing", [DialogPid]),
            RespSipMsg
    end.

-spec need_create_dialog(ersip_sipmsg:sipmsg()) -> boolean().
need_create_dialog(ReqSipMsg) ->
    ersip_sipmsg:method(ReqSipMsg) == ersip_method:invite().
