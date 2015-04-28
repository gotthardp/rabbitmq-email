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

email_header_test_() ->
    [?_assert(filter_email("t0001.txt", {<<"Subject">>, undefined})),
    ?_assert(filter_email("t0002.txt", {<<"Subject">>, <<"gobbledigook">>}))].

email_filtering_test_() ->
    [?_assert(filter_email("t0001.txt", {content_type, undefined})),
    ?_assert(filter_email("samples/messages/m0001.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0002.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0003.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0004.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0005.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0006.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0007.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0008.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0009.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0010.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0011.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0012.txt", {content_type, <<"image/png">>})),
    ?_assert(filter_email("samples/messages/m0013.txt", {content_type, <<"image/png">>})),
    ?_assert(filter_email("samples/messages/m0014.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0015.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0016.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0017.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m0018.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1001.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1002.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1003.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1004.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1005.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1006.txt", {content_type, <<"text/html">>})),
    ?_assert(filter_email("samples/messages/m1007.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1008.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1009.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1010.txt", {error})), % encoding mismatch
    ?_assert(filter_email("samples/messages/m1011.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1012.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1013.txt", {content_type, <<"image/png">>})),
    ?_assert(filter_email("samples/messages/m1014.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1015.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m1016.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2001.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2002.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2003.txt", {content_type, <<"text/html">>})),
    ?_assert(filter_email("samples/messages/m2004.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2005.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2006.txt", {content_type, <<"text/html">>})),
    ?_assert(filter_email("samples/messages/m2007.txt", {content_type, <<"text/html">>})),
    ?_assert(filter_email("samples/messages/m2008.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2009.txt", {content_type, <<"text/html">>})),
    ?_assert(filter_email("samples/messages/m2010.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2011.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2012.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2013.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2014.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2015.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m2016.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m3001.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m3002.txt", {content_type, <<"text/plain">>})),
    ?_assert(filter_email("samples/messages/m3003.txt", {content_type, <<"application/octet-stream">>})),
    ?_assert(filter_email("samples/messages/m3004.txt", {content_type, <<"text/plain">>}))].

filter_email(Filename, Expect) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    #'exchange.declare_ok'{}
        = amqp_channel:call(Channel, #'exchange.declare'{exchange = <<"email-in">>,
                                                         durable = true,
                                                         type = <<"topic">>}),
    #'queue.declare_ok'{queue = Queue}
        = amqp_channel:call(Channel, #'queue.declare'{auto_delete = true}),
    #'queue.bind_ok'{}
        = amqp_channel:call(Channel, #'queue.bind'{queue = Queue,
                                                   exchange = <<"email-in">>,
                                                   routing_key = <<"#">>}),
    % send e-mail
    {ok, File} = file:read_file("test/data/"++Filename),
    ?debugFmt("~s", [Filename]),
    Res = case gen_smtp_client:send_blocking({"whatever@example.com", ["test@example.com"], File},
                              [{relay, "127.0.0.1"}, {port, 2525}]) of
        Answer when is_binary(Answer) ->
            % expect AMQP message
            wait_for_message(Channel, Queue, 10, Expect);
        {error, Reason} ->
            expect_error(Reason, Expect);
        {error, _, Reason} ->
            expect_error(Reason, Expect)
    end,
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    Res.

wait_for_message(Channel, Queue, Wait, Expect) ->
    case amqp_channel:call(Channel, #'basic.get'{queue = Queue}) of
        {'basic.get_empty', _} ->
            timer:sleep(Wait),
            wait_for_message(Channel, Queue, 2*Wait, Expect);
        {#'basic.get_ok'{}, #amqp_msg{props=Props}} ->
            expect(Props, Expect)
    end.

expect(#'P_basic'{content_type=Expected}, {content_type, Expected}) ->
    true;
expect(#'P_basic'{content_type=Received}, {content_type, Expected}) ->
    ?debugFmt("Content-Type mismatch: expected ~s, received ~s", [Expected, Received]),
    false;

expect(#'P_basic'{headers = Headers}, {Header, undefined}) ->
    case lists:keyfind(Header, 1, Headers) of
        {_,_,Received} ->
            ?debugFmt("~s not expected: received ~s", [Header, Received]),
            false;
        false ->
            true
    end;

expect(#'P_basic'{headers = Headers}, {Header, Expected}) ->
    case lists:keyfind(Header, 1, Headers) of
        {_,_,Expected} ->
            true;
        {_,_,Received} ->
            ?debugFmt("~s mismatch: expected ~s, received ~s", [Header, Expected, Received]),
            false;
        false ->
            ?debugFmt("~s missing, expected ~s", [Header, Expected]),
            false
    end.

expect_error(_Reason, {error}) ->
    true;
expect_error(Reason, Expected) ->
    ?debugFmt("unexpected error, expected ~w, received ~s", [Expected, Reason]),
    false.

% end of file
