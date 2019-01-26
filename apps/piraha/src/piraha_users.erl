%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha Logic
%%% User table
%%%

-module(piraha_users).

-export([lookup/1]).

%%===================================================================
%% API
%%===================================================================

-spec lookup(ersip_uri:uri()) -> {ok, piraha_group:group()} | not_found.
lookup(_URI) ->
    Group0 = piraha_group:new({display_name, <<"\"els123 els123\"">>},
                             ersip_uri:make(<<"sip:+299381819@test.iserfik.gl;user=phone">>)),
    Group1 = piraha_group:add({display_name, []},
                              ersip_uri:make(<<"sip:341000@test.iserfik.gl;user=phone">>),
                              Group0),
    {ok, Group1}.

