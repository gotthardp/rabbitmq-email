%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_email_sender).

-export([send_email/5]).

send_email(To, Domain, {Type, Subtype}, Headers, Payload) ->
    ToAddr = construct_address(To, Domain),
    rabbit_log:info("sending ~p/~p e-mail to ~p~n", [Type, Subtype, ToAddr]),
    % add correct From and To headers
    {ok, From} = application:get_env(rabbitmq_email, email_from),
    Headers2 = Headers ++
        [{<<"From">>, construct_address(From, Domain)},
        {<<"To">>, ToAddr}],

    Headers3 = lists:foldr(fun set_header/2, [], Headers2),
    % Note FUTURE for gen_smtp 1.2.0:
    % mimemail:encode argument:
    % {Type, Subtype, Headers, ContentTypeParams, Parts}
    % https://github.com/gen-smtp/gen_smtp/pull/190
    % https://github.com/gen-smtp/gen_smtp/blob/1.1.1/src/mimemail.erl#L82-L89
    % https://github.com/gen-smtp/gen_smtp/blob/c6f25a758d60da9788bf5ddf73e985dacb28f74b/src/mimemail.erl#L98-L108
    Message = mimemail:encode({Type, Subtype, Headers3, [], Payload}),

    % client_sender must be a valid user, whereas From doesn't have to
    {ok, Sender} = application:get_env(rabbitmq_email, client_sender),
    {ok, ClientConfig} = application:get_env(rabbitmq_email, client_config),
    case gen_smtp_client:send({Sender, [ToAddr], Message}, ClientConfig) of
	{ok, _Pid} -> ok;
	{error, Res} -> rabbit_log:error("message cannot be sent: ~w~n", [Res])
    end.

set_header({Name, Binary}, Acc) when is_binary(Binary) -> [{Name, Binary}|Acc];
set_header({Name, List}, Acc) when is_list(List) -> [{Name, list_to_binary(List)}|Acc];
set_header({_Name, undefined}, Acc) -> Acc;
set_header({_Name, <<>>}, Acc) -> Acc;
set_header({_Name, []}, Acc) -> Acc.

construct_address(Addr, Domain) ->
    case binary:match(Addr, <<"@">>) of
        nomatch -> <<Addr/binary, $@, Domain/binary>>;
        _Else -> Addr
    end.

% end of file

