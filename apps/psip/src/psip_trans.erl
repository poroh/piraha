%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha SIP Stack
%%% SIP transaction
%%%

-module(psip_trans).

-behaviour(gen_server).

-export([start_link/1,
         server_process/2,
         server_response/2]).

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

-record(state,
        {trans :: ersip_trans:trans(),
         handler :: psip_handler:handler(),
         origmsg :: ersip_sipmsg:sipmsg()
        }).
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

-spec server_process(ersip_msg:message(), psip_handler:handler()) -> ok.
server_process(Msg, Handler) ->
    case ersip_sipmsg:parse(Msg, all_required) of
        {ok, SipMsg} ->
            case find_server(SipMsg) of
                {ok, Pid} ->
                    gen_server:cast(Pid, {received, SipMsg});
                error ->
                    psip_log:debug("transaction is not found: creating new one", []),
                    Args = [server, Handler, SipMsg],
                    case psip_trans_sup:start_child([Args]) of
                        {ok, _} -> ok;
                        {error, _} = Error ->
                            psip_log:error("failed to create transaction: ~p", [Error])
                    end
            end;
        {error, _} = Error ->
            psip_log:warning("failed to parse SIP message: ~p~n~s", [Error, ersip_msg:serialize(Msg)])
    end.

-spec server_response(ersip_sipmsg:sipmsg(), trans()) -> ok.
server_response(Resp, {trans, Pid}) ->
    gen_server:cast(Pid, {send, Resp}).

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

init([server, Handler, SipMsg]) ->
    {Trans, TransSE} = ersip_trans:new_server(SipMsg, #{}),
    TransId = ersip_trans:id(Trans),
    gproc:add_local_name(TransId),
    psip_log:debug("psip trans: starting server transaction with id: ~p", [TransId]),
    gen_server:cast(self(), {process_se, TransSE}),
    State = #state{trans   = Trans,
                   handler = Handler,
                   origmsg = SipMsg
                  },
    {ok, State}.

handle_call(Request, _From, State) ->
    psip_log:error("psip trans: unexpected call: ~p", [Request]),
    {reply, {error, {unexpected_call, Request}}, State}.

handle_cast({process_se, SE}, #state{handler = Handler, origmsg = SipMsg} = State) ->
    case process_se_list(SE, SipMsg, Handler) of
        continue ->
            {noreply, State};
        stop ->
            {stop, normal, State}
    end;
handle_cast({send, _} = Ev, #state{handler = Handler, origmsg = SipMsg, trans = Trans} = State) ->
    psip_log:debug("psip trans: sending message", []),
    {NewTrans, SE} = ersip_trans:event(Ev, Trans),
    NewState = State#state{trans = NewTrans},
    case process_se_list(SE, SipMsg, Handler) of
        continue ->
            {noreply, NewState};
        stop ->
            {stop, normal, NewState}
    end;
handle_cast({received, _} = Ev, #state{handler = Handler, origmsg = SipMsg, trans = Trans} = State) ->
    psip_log:debug("psip trans: received message", []),
    {NewTrans, SE} = ersip_trans:event(Ev, Trans),
    NewState = State#state{trans = NewTrans},
    case process_se_list(SE, SipMsg, Handler) of
        continue ->
            {noreply, NewState};
        stop ->
            {stop, normal, NewState}
    end;
handle_cast(Request, State) ->
    psip_log:error("psip trans: unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info({event, TimerEvent}, #state{handler = Handler, origmsg = SipMsg, trans = Trans} = State) ->
    psip_log:debug("psip trans: timer fired ~p", [TimerEvent]),
    {NewTrans, SE} = ersip_trans:event(TimerEvent, Trans),
    NewState = State#state{trans = NewTrans},
    case process_se_list(SE, SipMsg, Handler) of
        continue ->
            {noreply, NewState};
        stop ->
            {stop, normal, NewState}
    end;
handle_info(Msg, State) ->
    psip_log:error("psip trans: unexpected info: ~p", [Msg]),
    {noreply, State}.

terminate(Reason, #state{}) ->
    case Reason of
        normal ->
            psip_log:debug("psip trans: finished", []);
        _ ->
            psip_log:error("psip trans: finished with error: ~p", [Reason])
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec find_server(ersip_sipmsg:sipmsg()) -> {ok, pid()} | error.
find_server(SipMsg) ->
    TransId = ersip_trans:server_id(SipMsg),
    case gproc:lookup_local_name(TransId) of
        Pid when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            error
    end.

-spec process_se_list([ersip_trans_se:side_effect()],
                      ersip_sipmsg:sipmsg(),
                      psip_handler:handler()) -> continue | stop.
process_se_list([], _OrigSipMsg, _Handler) ->
    continue;
process_se_list([SE | Rest], OrigSipMsg, Handler) ->
    case process_se(SE, OrigSipMsg, Handler) of
        stop -> stop;
        continue ->
            process_se_list(Rest, OrigSipMsg, Handler)
    end.

-spec process_se(ersip_trans_se:side_effect(),
                ersip_sipmsg:sipmsg(),
                psip_handler:handler()) -> continue | stop.
process_se({tu_result, SipMsg}, _, Handler) ->
    psip_handler:transaction({trans, self()}, SipMsg, Handler),
    continue;
process_se({set_timer, {Timeout, TimerEvent}}, _ReqSipMsg, _Handler) ->
    psip_log:debug("psip trans: set timer on ~p ms: ~p", [Timeout, TimerEvent]),
    erlang:send_after(Timeout, self(), {event, TimerEvent}),
    continue;
process_se({clear_trans, Reason}, _ReqSipMsg, Handler) ->
    psip_log:debug("psip trans: transaction cleared: ~p", [Reason]),
    psip_handler:transaction_stop({trans, self()}, Reason, Handler),
    stop;
process_se({send_request, _OutReq}, _ReqSipMsg, _Handler) ->
    %% TODO:
    continue;
process_se({send_response, Response}, ReqSipMsg, _Handler) ->
    psip_log:debug("psip trans: sending response", []),
    psip_source:send_response(Response, ReqSipMsg),
    continue.
