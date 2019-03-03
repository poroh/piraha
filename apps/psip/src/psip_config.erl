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
         listen_port/0,
         exposed_address/0,
         exposed_port/0
        ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec listen_address() -> inet:ip_address().
listen_address() ->
    case application:get_env(psip, listen_address, auto) of
        auto ->
            first_non_loopack_address();
        {_, _, _, _} = Addr4 ->
            Addr4;
        {_, _, _, _, _, _, _, _} = Addr6 ->
            Addr6;
        {interface, IfName} ->
            first_address(IfName, ip4);
        {interface, Type, IfName} ->
            first_address(IfName, Type)
    end.

-spec listen_port() -> inet:port_number().
listen_port() ->
    application:get_env(psip, listen_port, 5060).

-spec exposed_address() -> inet:ip_address().
exposed_address() ->
    application:get_env(psip, exposed_address, listen_address()).

-spec exposed_port() -> inet:port_number().
exposed_port() ->
    application:get_env(psip, exposed_port, listen_port()).

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

-spec first_address(string(), ip4 | ip6) -> inet:ip_address().
first_address(IfName, Type) ->
    {ok, IfAddrs} = inet:getifaddrs(),
    Props = proplists:get_value(IfName, IfAddrs),
    Addrs = proplists:get_all_values(addr, Props),
    case Type of
        ip4 -> hd([A || {_, _, _, _} = A <- Addrs]);
        ip6 -> hd([A || {_, _, _, _, _, _, _, _} = A <- Addrs])
    end.

-spec is_loopback(inet:getifaddrs_ifopts()) -> boolean().
is_loopback(Props) ->
    Flags = proplists:get_value(flags, Props),
    lists:member(loopback, Flags).

-spec has_address(inet:getifaddrs_ifopts()) -> boolean().
has_address(Props) ->
    proplists:get_value(addr, Props) /= undefined.
