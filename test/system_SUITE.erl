%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2015-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% Copyright (C) 2017 Pivotal Software, Inc.
%

-module(system_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-compile(export_all).

all() ->
    [
     {group, non_parallel_tests}
    ].

groups() ->
    [
     {non_parallel_tests, [], [
                               email_header_test
                               , email_filtering_test
                               , email_filtering_hunny_samples_test
                              ]}
    ].

%% -------------------------------------------------------------------
%% Test suite setup/teardown
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
                                                    {rmq_nodename_suffix, ?MODULE}
                                                   ]),
    rabbit_ct_helpers:run_setup_steps(Config1,
                                      rabbit_ct_broker_helpers:setup_steps() ++
                                      rabbit_ct_client_helpers:setup_steps()).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
                                         rabbit_ct_client_helpers:teardown_steps() ++
                                         rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(_, Config) -> Config.

end_per_group(_, Config) -> Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase).

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -------------------------------------------------------------------
%% Test cases
%% -------------------------------------------------------------------



email_header_test(Config) ->
    [
     ?assert(filter_email(Config, "t0001.txt", {<<"Subject">>, undefined})),
     ?assert(filter_email(Config, "t0002.txt", {<<"Subject">>, <<"gobbledigook">>}))
    ].

email_filtering_test(Config) ->
    [
     ?assert(filter_email(Config, "t0001.txt", {content_type, undefined}))
    ].

email_filtering_hunny_samples_test(Config) ->
    [
     ?assert(filter_email(Config, "samples/messages/m0001.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0002.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0003.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0004.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0005.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0006.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0007.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0008.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0009.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0010.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0011.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0012.txt", {content_type, <<"image/png">>})),
     ?assert(filter_email(Config, "samples/messages/m0013.txt", {content_type, <<"image/png">>})),
     ?assert(filter_email(Config, "samples/messages/m0014.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0015.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0016.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0017.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m0018.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1001.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1002.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1003.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1004.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1005.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1006.txt", {content_type, <<"text/html">>})),
     ?assert(filter_email(Config, "samples/messages/m1007.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1008.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1009.txt", {content_type, <<"text/plain">>})),
     %% encoding mismatch
     ?assert(filter_email(Config, "samples/messages/m1010.txt", {error, {permanent_failure, "127.0.0.1", <<"554 Message cannot be delivered\r\n">>}})),
     ?assert(filter_email(Config, "samples/messages/m1011.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1012.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1013.txt", {content_type, <<"image/png">>})),
     ?assert(filter_email(Config, "samples/messages/m1014.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1015.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m1016.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2001.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2002.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2003.txt", {content_type, <<"text/html">>})),
     ?assert(filter_email(Config, "samples/messages/m2004.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2005.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2006.txt", {content_type, <<"text/html">>})),
     ?assert(filter_email(Config, "samples/messages/m2007.txt", {content_type, <<"text/html">>})),
     ?assert(filter_email(Config, "samples/messages/m2008.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2009.txt", {content_type, <<"text/html">>})),
     ?assert(filter_email(Config, "samples/messages/m2010.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2011.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2012.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2013.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2014.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2015.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m2016.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m3001.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m3002.txt", {content_type, <<"text/plain">>})),
     ?assert(filter_email(Config, "samples/messages/m3003.txt", {content_type, <<"application/octet-stream">>})),
     ?assert(filter_email(Config, "samples/messages/m3004.txt", {content_type, <<"text/plain">>}))
    ].

%%
%% Helpers
%%

filter_email(Config, Filename, Expect) ->
    Ch = rabbit_ct_client_helpers:open_channel(Config),
    amqp_channel:call(Ch, #'exchange.declare'{exchange = <<"email-in">>,
                                              durable = true,
                                              type = <<"fanout">>}),
    #'queue.declare_ok'{queue = Queue}
        = amqp_channel:call(Ch, #'queue.declare'{exclusive = true}),
    amqp_channel:call(Ch, #'queue.bind'{queue = Queue,
                                        exchange = <<"email-in">>,
                                        routing_key = <<"">>}),
    %% send an e-mail
    {ok, Email} = read_data_sample(Config, Filename),
    ct:pal("Read a sample from ~ts", [Filename]),
    Res = case gen_smtp_client:send_blocking({"whatever@example.com", ["test@example.com"], Email},
                [{relay, "127.0.0.1"}, {port, 2525}, {username, "guest"}, {password, "guest"}]) of
        Answer when is_binary(Answer) ->
            % expect a delivery
            ct:pal("SMTP server response: ~tp, will wait for a delivery...", [Answer]),
            wait_for_message(Ch, Queue, 500, Expect);
        {error, Reason} ->
            ct:pal("SMTP server reported an error: ~tp...", [Reason]),
            expect_error(Reason, Expect);
        {error, _, Reason} ->
            ct:pal("SMTP server reported an error: ~tp...", [Reason]),
            expect_error(Reason, Expect)
    end,
    rabbit_ct_client_helpers:close_channel(Ch),
    Res.

read_data_sample(Config, Filename) ->
    file:read_file(filename:join(?config(data_dir, Config), Filename)).

wait_for_message(Ch, Queue, Wait, Expect) ->
    case amqp_channel:call(Ch, #'basic.get'{queue = Queue}) of
        {'basic.get_empty', _} ->
            timer:sleep(Wait),
            wait_for_message(Ch, Queue, Wait, Expect);
        {#'basic.get_ok'{}, #amqp_msg{props = Props}} ->
            expect(Props, Expect)
    end.

expect(#'P_basic'{content_type = Expected}, {content_type, Expected}) ->
    true;
expect(#'P_basic'{content_type = Received}, {content_type, Expected}) ->
    ct:pal("Content-Type mismatch: expected ~ts, received ~ts", [Expected, Received]),
    false;

expect(#'P_basic'{headers = Headers}, {Header, undefined}) ->
    case lists:keyfind(Header, 1, Headers) of
        {_,_,Received} ->
            ct:pal("~ts not expected: received ~ts", [Header, Received]),
            false;
        false ->
            true
    end;

expect(#'P_basic'{headers = Headers}, {Header, Expected}) ->
    case lists:keyfind(Header, 1, Headers) of
        {_,_,Expected} ->
            true;
        {_,_,Received} ->
            ct:pal("~ts mismatch: expected ~ts, received ~ts", [Header, Expected, Received]),
            false;
        false ->
            ct:pal("~ts missing, expected ~ts", [Header, Expected]),
            false
    end;

expect(Props, error) ->
    ct:pal("unexpected success, expected error, received ~w", [Props]),
    false.

expect_error(_Reason, error) ->
    true;
expect_error(_Reason, {error, _}) ->
    true;
expect_error(Reason, Expected) ->
    ct:pal("unexpected error, expected ~tp, received ~tp", [Expected, Reason]),
    false.
