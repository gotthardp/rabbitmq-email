%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
% Copyright (c) 2021 VMware, Inc. or its affiliates.  All rights reserved.
%

-module(rabbitmq_email_app).

-behaviour(application).
-export([start/2, stop/1]).

-behaviour(supervisor).
-export([init/1]).

start(_Type, _StartArgs) ->
    supervisor:start_link(?MODULE, []).
    
stop(_State) ->
    ok.

init([]) ->
    {ok, ServerConfig} = application:get_env(rabbitmq_email, server_config),

    % email to amqp
    SmtpServerSpec = gen_smtp_server:child_spec(email_handler, rabbit_email_handler, ServerConfig),

    % amqp to email
    MessageHandlerSpec = {message_handler_sup, {rabbit_message_handler_sup, start_link, []}, permanent, 10000, supervisor, [rabbit_message_handler_sup]},

    Specs = [SmtpServerSpec, MessageHandlerSpec],

    Flags = #{
        strategy  => one_for_one,
        intensity => 3,
        period    => 10
    },
    {ok, {Flags, Specs}}.
