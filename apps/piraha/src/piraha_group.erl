%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha Logic
%%% Hunting group
%%%

-module(piraha_group).

-export([new/2,
         add/3,
         next/1
        ]).

-export_type([group/0, user/0]).

%%===================================================================
%% Types
%%===================================================================

-record(group, {owner        :: user(),
                members = [] :: [user()]
               }).
-type user()   :: {ersip_nameaddr:display_name(), ersip_uri:uri()}.
-type group()  :: #group{}.

%%===================================================================
%% API
%%===================================================================

-spec new(ersip_nameaddr:display_name(), ersip_uri:uri()) -> group().
new(OwnerDisplayName, OwnerURI) ->
    #group{owner = make_user(OwnerDisplayName, OwnerURI)}.

-spec add(ersip_nameaddr:display_name(), ersip_uri:uri(), group()) -> group().
add(DisplayName, URI, #group{} = Group) ->
    Group#group{members = Group#group.members ++ [make_user(DisplayName, URI)]}.

-spec next(group()) -> not_found | {ok, user(), group()}.
next(#group{members = []}) ->
    not_found;
next(#group{members = [Next | Rest]} = Group) ->
    {ok, Next, Group#group{members = Rest}}.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec make_user(ersip_nameaddr:display_name(), ersip_uri:uri()) -> user().
make_user(DN, URI) ->
    {DN, URI}.
