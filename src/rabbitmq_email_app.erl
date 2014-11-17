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
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).
    
stop(_State) ->
    ok.

init([]) ->
    {ok, {{one_for_one, 3, 10},
        [{rabbit_message_handler, {rabbit_message_handler, start_link, []},
            permanent, 10000, worker, [mailer]},
        {rabbit_email_handler, {gen_smtp_server, start_link, [rabbit_email_handler,
            application:get_env(rabbitmq_email, server_config, [])]},
            permanent, 10000, worker, [mailer]}
        ]}}.

% end of file

