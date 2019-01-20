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
lookup(_) ->
    not_found.

