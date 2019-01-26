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
         transaction/3,
         transaction_stop/3,
         uas_request/3
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

-spec transaction(psip_trans:trans(), ersip_sipmsg:sipmsg(), any()) -> process_uas.
transaction(Trans, _SipMsg, _) ->
    psip_log:debug("new request within transaction: ~p: process as UAS", [Trans]),
    process_uas.

-spec uas_request(psip_uas:uas(), ersip_sipmsg:sipmsg(), any()) -> ok.
uas_request(UAS, ReqSipMsg, _) ->
    RURI = ersip_sipmsg:ruri(ReqSipMsg),
    RURIIO = ersip_uri:assemble(RURI),
    psip_log:debug("new UAS request: ~s", [RURIIO]),
    case piraha_users:lookup(RURI) of
        {ok, Group} ->
            piraha_hunt:start(UAS, ReqSipMsg, Group);
        not_found ->
            psip_log:warning("cannot find hunt group for URI: ~s", [RURIIO]),
            NotFoundResp = ersip_sipmsg:reply(404, ReqSipMsg),
            psip_uas:response(NotFoundResp, UAS)
    end.

-spec transaction_stop(psip_trans:trans(), ersip_sipmsg:sipmsg(), any()) -> ok.
transaction_stop(Trans, Reason, _) ->
    psip_log:debug("transaction ~p stopped with reason: ~p", [Trans, Reason]),
    ok.
