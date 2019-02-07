%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha SIP Stack
%%% SIP Back-to-back user agent
%%%

-module(psip_b2bua).

-export([start_link/1,
         join_dialogs/2,
         process_ack/1
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

-record(state, {ids :: {ersip_dialog:id(), ersip_dialog:id()}}).
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

-spec join_dialogs(ersip_dialog:id(), ersip_dialog:id()) -> ok.
join_dialogs(DialogId1, DialogId2) ->
    Args = {DialogId1, DialogId2},
    {ok, _} = psip_b2bua_sup:start_child([Args]),
    ok.

-spec process_ack(ersip_sipmsg:sipmsg()) -> ok | not_found.
process_ack(SipMsg) ->
    case ersip_sipmsg:dialog_id(uas, SipMsg) of
        no_dialog -> not_found;
        {ok, DialogId} ->
            case find_b2bua(DialogId) of
                {ok, Pid} ->
                    gen_server:cast(Pid, {pass_ack, DialogId, SipMsg});
                not_found ->
                    not_found
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

init({DialogId1, DialogId2}) ->
    gproc:add_local_name({b2bua, DialogId1}),
    gproc:add_local_name({b2bua, DialogId2}),
    psip_log:debug("b2bua: started by with ids: ~p ~p", [DialogId1, DialogId2]),
    State = #state{ids = {DialogId1, DialogId2}},
    {ok, State}.

handle_call(Request, _From, State) ->
    psip_log:error("b2bua: unexpected call: ~p", [Request]),
    {reply, {error, {unexpected_call, Request}}, State}.

handle_cast({pass_ack, SrcDialogId, AckSipMsg}, #state{} = State) ->
    DstDialogId = another_dialog_id(SrcDialogId, State#state.ids),
    psip_log:debug("b2bua: passing ACK to: ~p", [DstDialogId]),
    psip_dialog:uac_request(DstDialogId, AckSipMsg),
    {noreply, State};
handle_cast(Request, State) ->
    psip_log:error("b2bua: unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info(Msg, State) ->
    psip_log:error("b2bua: unexpected info: ~p", [Msg]),
    {noreply, State}.

terminate(Reason, #state{}) ->
    case Reason of
        normal ->
            psip_log:debug("b2bua: finished", []);
        _ ->
            psip_log:error("b2bua: finished with error: ~p", [Reason])
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec find_b2bua(ersip_dialog:id()) -> {ok, pid()} | not_found.
find_b2bua(DialogId) ->
    case gproc:lookup_local_name({b2bua, DialogId}) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        undefined ->
            not_found
    end.

-spec another_dialog_id(ersip_dialog:id(), {ersip_dialog:id(), ersip_dialog:id()}) -> ersip_dialog:id().
another_dialog_id(A, {A, B}) ->
    B;
another_dialog_id(B, {A, B}) ->
    A.
