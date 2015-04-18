%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_email_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

email_filtering_test() ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    #'exchange.declare_ok'{}
        = amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"email-in">>,
                                                         type = <<"topic">>}),
    #'queue.declare_ok'{queue = Q}
        = amqp_channel:call(Channel, #'queue.declare'{auto_delete = true}),
    #'queue.bind_ok'{}
        = amqp_channel:call(Channel, #'queue.bind'{queue = Q,
                                                   exchange = <<"email-in">>,
                                                   routing_key = <<"#">>}),

    gen_smtp_client:send({"whatever@example.com", ["test@example.com"],
        "Content-Type: text/plain\r\nFrom: Andrew Thompson \r\nTo: Some Dude \r\n\r\nThis is the email body"},
        [{relay, "127.0.0.1"}, {port, 2525}]),

    timer:sleep(100),
    case amqp_channel:call(Channel, #'basic.get'{queue = Q, no_ack = true}) of
        {'basic.get_empty', _} -> exit(didnt_receive_message);
        {#'basic.get_ok'{}, #amqp_msg{props=Props}} ->
            #'P_basic'{content_type = <<"text/plain">>} = Props
    end,
    ok.
