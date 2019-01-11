%%
%% Copyright (c) 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Piraha SIP Stack
%% SIP port
%%

-module(psip_udp_port).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% Types
%%%===================================================================

-record(state, {local_ip   :: inet:ip_address(),
                local_port :: inet:port_number(),
                socket    :: gen_udp:socket()}).
-type state() :: #state{}.
-type start_link_ret() :: {ok, pid()} |
                          {error, {already_started, pid()}} |
                          {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> start_link_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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

init([]) ->
    IPAddress = psip_config:listen_address(),
    Port = psip_config:listen_port(),
    psip_log:notice("psip udp port: starting at ~s:~p", [inet:ntoa(IPAddress), Port]),
    case gen_udp:open(Port, [binary, {ip, IPAddress}, {active, once}]) of
        {error, _} = Error ->
            psip_log:error("psip udp port: failed to open port: ~p", [Error]),
            {stop, Error};
        {ok, Socket} ->
            State = #state{local_ip = IPAddress,
                           local_port = Port,
                           socket = Socket},
            {ok, State}
    end.

handle_call(Request, _From, State) ->
    psip_log:error("psip udp port: unexpected call: ~p", [Request]),
    {reply, {error, {unexpected_call, Request}}, State}.

handle_cast(Request, State) ->
    psip_log:error("psip udp port: unexpected cast: ~p", [Request]),
    {noreply, State}.

handle_info({udp, Socket, IP, Port, Msg}, #state{socket=Socket} = State) ->
    psip_log:debug("psip udp port: new message:~n~s", [Msg]),
    recv_message(IP, Port, Msg, State),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, State};
handle_info(Msg, State) ->
    psip_log:error("psip udp port: unexpected info: ~p", [Msg]),
    {noreply, State}.

terminate(normal, #state{socket = Socket}) ->
    psip_log:notice("psip udp port: stopped", []),
    gen_udp:close(Socket);
terminate(Reason, #state{socket = Socket}) ->
    psip_log:error("psip udp port: stopped with reason ~p", [Reason]),
    gen_udp:close(Socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec recv_message(inet:ip_address(), inet:port_number(), binary(), state()) -> ok.
recv_message(RemoteIP, RemotePort, Message, State) ->
    Conn = ersip_conn:new(State#state.local_ip,
                          State#state.local_port,
                          RemoteIP,
                          RemotePort,
                          ersip_transport:udp(),
                          #{}),
    {_, ConnSE} = ersip_conn:conn_data(Message, Conn),
    process_side_effects(ConnSE, State),
    ok.

-spec process_side_effects([ersip_conn_se:side_effect()], state()) -> ok.
process_side_effects([], _State) ->
    ok;
process_side_effects([E|Rest], State) ->
    process_side_effect(E, State),
    process_side_effects(Rest, State).

-spec process_side_effect(ersip_conn_se:side_effect(), state()) -> ok.
process_side_effect({bad_message, Data, Error}, _State) ->
    psip_log:warning("psip udp port: bad message received: ~p~n~s", [Error, Data]);
process_side_effect({new_request, _Msg}, _State) ->
    psip_log:debug("psip udp port: process new request", []);
process_side_effect({new_response, _Msg}, _State) ->
    psip_log:debug("psip udp port: process new response", []).
