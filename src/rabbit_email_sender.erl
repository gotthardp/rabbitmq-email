%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_email_sender).

-include_lib("amqp_client/include/amqp_client.hrl").
-export([send_email/4]).

send_email(Address, Domain, Properties, Payload) ->
    {Type, Subtype} = get_content_type(Properties#'P_basic'.content_type),

    Message = mimemail:encode({Type, Subtype,
        % headers
        set_plain_header(<<"From">>, <<"noreply", $@, Domain/binary>>)++
        set_plain_header(<<"Message-Id">>, Properties#'P_basic'.message_id),
        % parameters
        [],
        % message body
        Payload}),

    {ok, Sender} = application:get_env(rabbitmq_email, client_sender),
    {ok, ClientConfig} = application:get_env(rabbitmq_email, client_config),
    case gen_smtp_client:send({Sender, [Address], Message}, ClientConfig) of
	{ok, _Pid} -> ok;
	{error, Res} -> rabbit_log:error("message cannot be sent: ~w~n", [Res])
    end.

get_content_type(undefined) ->
    {<<"text">>, <<"plain">>};
get_content_type(Binary) ->
    [Type, Subtype|_Others] = binary:split(Binary, [<<$/>>, <<$;>>], [global]),
    {Type, Subtype}.

set_plain_header(_Name, undefined) -> [];
set_plain_header(Name, Binary) -> [{Name, Binary}].

% end of file

