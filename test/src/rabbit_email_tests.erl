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

email_filtering_test_() ->
    [?_assert(filter_email("t0001.txt") == undefined),
    ?_assert(filter_email("samples/messages/m0001.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0002.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0003.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0004.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0005.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0006.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0007.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0008.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0009.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0010.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0011.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0012.txt") == <<"image/png">>),
    ?_assert(filter_email("samples/messages/m0013.txt") == <<"image/png">>),
    ?_assert(filter_email("samples/messages/m0014.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0015.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0016.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0017.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m0018.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1001.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1002.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1003.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1004.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1005.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1006.txt") == <<"text/html">>),
    ?_assert(filter_email("samples/messages/m1007.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1008.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1009.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1010.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1011.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1012.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1013.txt") == <<"image/png">>),
    ?_assert(filter_email("samples/messages/m1014.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1015.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m1016.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2001.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2002.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2003.txt") == <<"text/html">>),
    ?_assert(filter_email("samples/messages/m2004.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2005.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2006.txt") == <<"text/html">>),
    ?_assert(filter_email("samples/messages/m2007.txt") == <<"text/html">>),
    ?_assert(filter_email("samples/messages/m2008.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2009.txt") == <<"text/html">>),
    ?_assert(filter_email("samples/messages/m2010.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2011.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2012.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2013.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2014.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2015.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m2016.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m3001.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m3002.txt") == <<"text/plain">>),
    ?_assert(filter_email("samples/messages/m3003.txt") == <<"application/octet-stream">>),
    ?_assert(filter_email("samples/messages/m3004.txt") == <<"text/plain">>)].

filter_email(Filename) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    #'exchange.declare_ok'{}
        = amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"email-in">>,
                                                         type = <<"topic">>}),
    #'queue.declare_ok'{queue = Queue}
        = amqp_channel:call(Channel, #'queue.declare'{auto_delete = true}),
    #'queue.bind_ok'{}
        = amqp_channel:call(Channel, #'queue.bind'{queue = Queue,
                                                   exchange = <<"email-in">>,
                                                   routing_key = <<"#">>}),
    % send e-mail
    {ok, File} = file:read_file("test/data/"++Filename),
    gen_smtp_client:send({"whatever@example.com", ["test@example.com"], File},
        [{relay, "127.0.0.1"}, {port, 2525}]),

    % expect AMQP message
    Res = wait_for_message(Channel, Queue, 10),
    ?debugFmt("~s ~s", [Filename, Res]),

    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    Res.

wait_for_message(Channel, Queue, Wait) ->
    case amqp_channel:call(Channel, #'basic.get'{queue = Queue}) of
        {'basic.get_empty', _} ->
            timer:sleep(Wait),
            wait_for_message(Channel, Queue, 2*Wait);
        {#'basic.get_ok'{}, #amqp_msg{props=Props}} ->
            #'P_basic'{content_type = Type} = Props,
            Type
    end.

% end of file
