%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha Logic
%%% SIP stack callbacks
%%%

-module(piraha_psip_callbacks).

-export([setup/0]).
-export([transp_request/2,
         transaction/3
        ]).

%%===================================================================
%% API
%%===================================================================

-spec setup() -> ok.
setup() ->
    Handler = psip_handler:new(?MODULE, any),
    psip_udp_port:set_handler(Handler),
    ok.

%%===================================================================
%% psip_callbacks
%%===================================================================

-spec transp_request(ersip_msg:message(), any()) -> start_transaction.
transp_request(_Msg, _) ->
    process_transaction.

-spec transaction(pid(), ersip_sipmsg:sipmsg(), any()) -> ok.
transaction(_Pid, _SipMsg, _) ->
    psip_log:debug("new request within transaction", []),
    ok.

