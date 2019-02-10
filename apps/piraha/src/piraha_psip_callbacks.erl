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
         uas_request/3,
         uas_cancel/2
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
uas_request(UAS, ReqSipMsg0, _) ->
    case dec_max_forwards(ReqSipMsg0) of
        {reply, Resp} ->
            psip_uas:response(Resp, UAS);
        {ok, ReqSipMsg} ->
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
            end
    end.

-spec uas_cancel(psip_uas:id(), any()) -> ok.
uas_cancel(UASId, _) ->
    piraha_hunt:cancel(UASId).

-spec transaction_stop(psip_trans:trans(), ersip_sipmsg:sipmsg(), any()) -> ok.
transaction_stop(Trans, Reason, _) ->
    psip_log:debug("transaction ~p stopped with reason: ~p", [Trans, Reason]),
    ok.

%%===================================================================
%% Internal implementation:
%%===================================================================

-spec dec_max_forwards(ersip_sipmsg:sipmsg()) -> {ok, ersip_sipmsg:sipmsg()} | {reply, ersip_sipmsg:sipmsg()}.
dec_max_forwards(SipMsg) ->
    case ersip_sipmsg:find(maxforwards, SipMsg) of
        {ok, MaxForwardsHdr0} ->
            case ersip_hdr_maxforwards:value(MaxForwardsHdr0) of
                X when X == 0 ->
                    {reply, ersip_sipmsg:reply(483, SipMsg)};
                _ ->
                    MaxForwardsHdr = ersip_hdr_maxforwards:dec(MaxForwardsHdr0),
                    {ok, ersip_sipmsg:set(maxforwards, MaxForwardsHdr, SipMsg)}
            end;
        not_found ->
            MaxForwardsHdr = ersip_hdr_maxforwards:make(70),
            {ok, ersip_sipmsg:set(maxforwards, MaxForwardsHdr, SipMsg)}
    end.
