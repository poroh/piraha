%%
%% Copyright (c) 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Piraha SIP Stack
%% Configuration
%%

-module(psip_config).

-export([listen_address/0,
         listen_port/0
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec listen_address() -> inet:ip_address().
listen_address() ->
    case application:get_env(psip, listen_address, auto) of
        auto ->
            first_non_loopack_address();
        Addr ->
            Addr
    end.

-spec listen_port() -> inet:port_number().
listen_port() ->
    application:get_env(psip, listen_port, 5060).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec first_non_loopack_address() -> inet:ip_address().
first_non_loopack_address() ->
    {ok, IfAddrs} = inet:getifaddrs(),
    Candidates = [proplists:get_value(addr, Props) || {_IfName, Props} <- IfAddrs,
                                                      not is_loopback(Props),
                                                      has_address(Props)],
    [First | _ ] = lists:sort(Candidates),
    First.

-spec is_loopback(inet:getifaddrs_ifopts()) -> boolean().
is_loopback(Props) ->
    Flags = proplists:get_value(flags, Props),
    lists:member(loopback, Flags).

-spec has_address(inet:getifaddrs_ifopts()) -> boolean().
has_address(Props) ->
    proplists:get_value(addr, Props) /= undefined.
