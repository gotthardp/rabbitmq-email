%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_message_handler).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-record(state, {channel}).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),

    lists:foreach(
        fun({{VHost, Queue}, Domain}) ->
            Subscribe = #'basic.consume'{queue=Queue, consumer_tag=Domain, no_ack=true},
            #'basic.consume_ok'{} = amqp_channel:call(Channel, Subscribe)
        end,
        application:get_env(rabbitmq_email, email_queues, [])),

    State = #state{channel = Channel},
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast({Reference, Domain, To, ContentType, Body}, #state{channel=Channel}=State) ->
    {VHost, Exchange} = proplists:get_value(list_to_binary(Domain),
        application:get_env(rabbitmq_email, email_domains, [])),

    lists:foreach(
        fun(Address) ->
            Publish = #'basic.publish'{exchange=Exchange, routing_key=Address},
            Properties = #'P_basic'{delivery_mode = 2, %% persistent message
                content_type = ContentType,
                message_id = list_to_binary(Reference),
                timestamp = amqp_ts()},
            Msg = #amqp_msg{props=Properties, payload=Body},
            amqp_channel:cast(Channel, Publish, Msg)
        end, To),
    {noreply, State}.

%% This is the first message received
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

%% This is received when the subscription is cancelled
handle_info(#'basic.cancel_ok'{}, State) ->
    {noreply, State};

%% A delivery
handle_info({#'basic.deliver'{routing_key=Key, consumer_tag=Tag}, Content}, State) ->
    #amqp_msg{props = Properties, payload = Payload} = Content,

    rabbit_email_sender:send_email(
        construct_address(Key, Tag), Tag, Properties, Payload),
    {noreply, State};

handle_info(Msg, State) ->
    rabbit_log:info("~w", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{channel = Channel}) ->
    amqp_channel:call(Channel, #'channel.close'{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

construct_address(Key, Tag) ->
    case binary:match(Key, <<"@">>) of
        nomatch -> <<Key/binary, $@, Tag/binary>>;
        _Else -> Key
    end.

amqp_ts() ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    MegaSecs * 1000 * 1000 + Secs.

% end of file

