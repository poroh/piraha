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
         start_link/1
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

-record(state, {srv_trans   :: psip_trans:trans(),
                request     :: ersip_sipmsg:sipmsg(),
                fwd_request :: ersip_sipmsg:sipmsg() | undefined,
                group       :: piraha_group:group(),
                clnt_trans  :: psip_trans:trans()
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

-spec start(psip_trans:trans(), ersip_sipmsg:sipmsg(), piraha_group:group()) -> ok.
start(ServerTrans, OrigSipMsg, Group) ->
    Args = [ServerTrans, OrigSipMsg, Group],
    case piraha_hunt_sup:start_child([Args]) of
        {ok, _} ->
            ok;
        {error, _} = Error ->
            psip_log:error("cannot start hunting process: ~p", [Error])
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



init([ServerTrans, ReqSipMsg, Group]) ->
    State = #state{srv_trans = ServerTrans,
                   request = ReqSipMsg,
                   group = Group},
    gen_server:cast(self(), start_hunt),
    {ok, State}.

handle_call(Request, _From, State) ->
    psip_log:error("piraha hunt: unexpected call: ~p", [Request]),
    {reply, {error, {unexpected_call, Request}}, State}.

handle_cast(start_hunt, #state{request = Req} = State) ->
    ProxyOptions  = proxy_options(),
    ValidateOptions = #{},
    Options = #{validate => ValidateOptions,
                proxy    => ProxyOptions},
    case ersip_proxy_common:request_validation(Req, Options) of
        {ok, SipMsg1} ->
            FwdReq = ersip_proxy_common:process_route_info(SipMsg1, ProxyOptions),
            NewState = State#state{fwd_request = FwdReq},
            gen_server:cast(self(), hunt_next),
            {noreply, NewState};
        {reply, Resp} ->
            psip_trans:server_response(Resp, State#state.srv_trans)
    end;
handle_cast(hunt_next, #state{} = State) ->
    case piraha_group:next(State#state.group) of
        not_found ->
            psip_log:warning("piraha hunt: cannot find next target: give up", []),
            NotFoundResp = ersip_sipmsg:reply(404, State#state.request),
            psip_trans:server_response(NotFoundResp, State#state.srv_trans),
            {stop, normal, State};
        {ok, {DN, URI}, Group1} ->
            psip_log:info("piraha hunt: diversion to ~p ~p", [DN, URI]),
            ProxyOptions = proxy_options(),
            {FwdMsg, _} = ersip_proxy_common:forward_request(URI, State#state.fwd_request, ProxyOptions),
            psip_trans:client_trans
            {noreply, State#state{group = Group1}}
    end;
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

-spec proxy_options() -> ersip_proxy:options().
proxy_options() ->
    #{}.
