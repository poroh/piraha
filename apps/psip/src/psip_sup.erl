%%
%% Copyright (c) 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Piraha SIP Stack
%% Application supervisor
%%

-module(psip_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Types
%%====================================================================

-type start_link_ret() :: {ok, pid()} |
                          {error, {already_started, pid()}} |
                          {error, term()}.

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(term()) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}
    | ignore.
init([]) ->
    SupFlags =
        #{strategy  => one_for_one,
          intensity => 1,
          period    => 5
         },
    ChildSpecs =
        [#{id       => psip_udp_port,
           start    => {psip_udp_port, start_link, []},
           type     => worker,
           restart  => permanent
          }
        ],
    {ok, {SupFlags, ChildSpecs}}.

