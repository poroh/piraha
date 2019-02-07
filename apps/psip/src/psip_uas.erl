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
         response/2
        ]).
-export_type([uas/0]).

%%===================================================================
%% Types
%%===================================================================

-type uas() :: {uas, psip_trans:trans(), ersip_sipmsg:sipmsg()}.

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
                    psip_handler:uas_request(UAS, ReqSipMsg1, Handler)
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
response(RespSipMsg0, {uas, Trans, ReqSipMsg}) ->
    RespSipMsg = psip_dialog:uas_response(RespSipMsg0, ReqSipMsg),
    psip_trans:server_response(RespSipMsg, Trans).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec uas_options() -> ersip_uas:options().
uas_options() ->
    #{check_scheme => fun check_scheme/1}.

allowed_methods() ->
    ersip_method_set:invite_set().

make_uas(ReqSipMsg, Trans) ->
    {uas, Trans, ReqSipMsg}.

-spec check_scheme(binary()) -> boolean().
check_scheme(<<"sip">>) -> true;
check_scheme(<<"sips">>) -> true;
check_scheme(<<"tel">>) -> true;
check_scheme(_) -> false.
