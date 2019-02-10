%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Piraha SIP Stack
%%% UAS
%%%

-module(psip_uas).

-export([process/3,
         process_ack/2,
         response/2,
         sipmsg/1,
         make_reply/3
        ]).
-export_type([uas/0]).

%%===================================================================
%% Types
%%===================================================================

-record(uas, {trans    :: psip_trans:trans(),
              req      :: ersip_sipmsg:sipmsg(),
              resp_tag :: ersip_hdr_fromto:tag()
             }).
-type uas() :: #uas{}.

%%===================================================================
%% API
%%===================================================================

-spec process(psip_trans:trans(), ersip_sipmsg:sipmsg(), psip_handler:handler()) -> ok.
process(Trans, ReqSipMsg0, Handler) ->
    case ersip_uas:process_request(ReqSipMsg0, allowed_methods(), uas_options()) of
        {reply, Resp} ->
            psip_trans:server_response(Resp, Trans);
        {process, ReqSipMsg1} ->
            case psip_dialog:uas_request(ReqSipMsg1) of
                {reply, Resp} ->
                    psip_trans:server_response(Resp, Trans);
                process ->
                    UAS = make_uas(ReqSipMsg1, Trans),
                    case psip_b2bua:process(UAS) of
                        ok -> ok;
                        not_found ->
                            psip_handler:uas_request(UAS, ReqSipMsg1, Handler)
                    end
            end
    end.

-spec process_ack(ersip_sipmsg:sipmsg(), psip_handler:handler()) -> ok.
process_ack(ReqSipMsg, Handler) ->
    case psip_dialog:uas_find(ReqSipMsg) of
        {ok, _} ->
            case psip_b2bua:process_ack(ReqSipMsg) of
                ok -> ok;
                not_found ->
                    psip_handler:process_ack(ReqSipMsg, Handler)
            end;
        not_found ->
            psip_log:warning("uas: cannot find dialog for ACK", []),
            ok
    end.

-spec response(ersip_sipmsg:sipmsg(), psip_trans:trans()) -> ok.
response(RespSipMsg0, #uas{trans = Trans, req = ReqSipMsg}) ->
    RespSipMsg = psip_dialog:uas_response(RespSipMsg0, ReqSipMsg),
    psip_trans:server_response(RespSipMsg, Trans).

-spec sipmsg(uas()) -> ersip_sipmsg:sipmsg().
sipmsg(#uas{req = ReqSipMsg}) ->
    ReqSipMsg.

-spec make_reply(ersip_status:code(), binary(), uas()) -> ersip_reply:options().
make_reply(Code, ReasonPhrase, #uas{resp_tag = Tag}) ->
    ersip_reply:new(Code,
                    [{reason, ReasonPhrase},
                     {to_tag, Tag}]).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec uas_options() -> ersip_uas:options().
uas_options() ->
    #{check_scheme => fun check_scheme/1}.

allowed_methods() ->
    ersip_method_set:invite_set().

make_uas(ReqSipMsg, Trans) ->
    #uas{trans = Trans,
         req = ReqSipMsg,
         resp_tag = {tag, ersip_id:token(crypto:strong_rand_bytes(6))}
        }.

-spec check_scheme(binary()) -> boolean().
check_scheme(<<"sip">>) -> true;
check_scheme(<<"sips">>) -> true;
check_scheme(<<"tel">>) -> true;
check_scheme(_) -> false.
