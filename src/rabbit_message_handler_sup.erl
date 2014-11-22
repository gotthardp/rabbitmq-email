%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_message_handler_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, _Arg = []).

init([]) ->
    {ok, {{one_for_one, 3, 10}, child_spec()}}.

child_spec() ->
    {ok, Queues} = application:get_env(rabbitmq_email, email_queues),
    lists:map(
        fun({{VHost, Queue}, Domain}) ->
            {list_to_atom("message_handler"++binary_to_list(Domain)),
                {rabbit_message_handler, start_link, [{VHost, Queue}, Domain]},
                permanent, 10000, worker, [rabbit_message_handler]}
        end, Queues).

% end of file

