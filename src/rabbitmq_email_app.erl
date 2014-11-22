%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbitmq_email_app).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

start(normal, []) ->
    supervisor:start_link(?MODULE, _Arg = []).
    
stop(_State) ->
    ok.

init([]) ->
    {ok, ServerConfig} = application:get_env(rabbitmq_email, server_config),
    {ok, {{one_for_one, 3, 10},
        % email to amqp
        [{email_handler, {gen_smtp_server, start_link, [rabbit_email_handler, ServerConfig]},
            permanent, 10000, worker, [rabbit_email_handler]},
        % amqo to email
        {message_handler_sup, {rabbit_message_handler_sup, start_link, []},
            permanent, 10000, supervisor, [rabbit_message_handler_sup]}]}}.

% end of file

