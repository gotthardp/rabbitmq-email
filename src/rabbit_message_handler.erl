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

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_continue/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {connection, channel}).

%% This gives RabbitMQ 60 seconds to reach core_started
-define(DEFAULT_TRIES, 12).
-define(RETRY_DELAY_MS, 5000).

start_link({VHost, Queue}, Domain) ->
    gen_server:start_link(?MODULE, [{VHost, Queue}, Domain], []).

init([{VHost, Queue}, Domain]) ->
    process_flag(trap_exit, true),
    {ok, #state{}, {continue, {try_connect, {VHost, Queue, Domain}}}}.

handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_continue({try_connect, Args}, State0) ->
    {ok, State1} = try_start_connection(Args, ?DEFAULT_TRIES, State0),
    {noreply, State1}.

handle_info({retry_connect, _Args, 0}, State) ->
    {stop, connection_failed, State};
handle_info({retry_connect, Args, RemainingTries}, State0) ->
    {ok, State1} = try_start_connection(Args, RemainingTries, State0),
    {noreply, State1};

%% This is the first message received
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

%% This is received when the subscription is cancelled
handle_info(#'basic.cancel_ok'{}, State) ->
    {noreply, State};

%% A delivery
handle_info({#'basic.deliver'{routing_key=Key, consumer_tag=Tag}, Content}, State) ->
    #amqp_msg{props = Properties, payload = Payload} = Content,
    #'P_basic'{message_id = MessageId, headers = Headers} = Properties,
    {Type, Subtype} = get_content_type(Properties#'P_basic'.content_type),

    Headers2 = transform_headers_to_email(Headers) ++
        [{<<"Message-Id">>, MessageId}],
    rabbit_email_sender:send_email(Key, Tag, {Type, Subtype}, Headers2, Payload),
    {noreply, State};

handle_info(Msg, State) ->
    rabbit_log:info("~w", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{connection=undefined, channel=undefined}) ->
    ok;
terminate(_Reason, #state{connection=Connection, channel=Channel}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

try_start_connection({VHost, Queue, Domain} = Args, RemainingTries, State0) ->
    rabbit_log:debug("~p starting connection to vhost '~p'", [?MODULE, VHost]),
    State1 = case amqp_connection:start(#amqp_params_direct{virtual_host=VHost}) of
        {error, broker_not_found_on_node} ->
            rabbit_log:warning("~p: broker_not_found_on_node seen, retrying in 1 second", [?MODULE]),
            retry_start_connection(Args, RemainingTries),
            State0;
        {error, What} ->
            rabbit_log:error("~p: error starting direct connection to vhost '~p', '~p'", [?MODULE, VHost, What]),
            retry_start_connection(Args, RemainingTries),
            State0;
        {ok, Connection} ->
            rabbit_log:debug("~p successfully started connection to vhost '~p'", [?MODULE, VHost]),
            try_declaring_queue(Connection, Queue),
            {ok, Channel} = amqp_connection:open_channel(Connection),
            Subscribe = #'basic.consume'{queue=Queue, consumer_tag=Domain, no_ack=true},
            #'basic.consume_ok'{} = amqp_channel:call(Channel, Subscribe),
            _State1 = State0#state{connection=Connection, channel=Channel}
    end,
    {ok, State1}.

try_declaring_queue(Connection, Queue) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    try
        catch amqp_channel:call(Channel, #'queue.declare'{queue=Queue, durable=true})
    after
        catch amqp_channel:close(Channel)
    end.

retry_start_connection(Args, RemainingTries) ->
    _Ref = erlang:send_after(?RETRY_DELAY_MS, self(), {retry_connect, Args, RemainingTries -1}).

get_content_type(undefined) ->
    {<<"text">>, <<"plain">>};
get_content_type(Binary) ->
    [Type, Subtype|_Others] = binary:split(Binary, [<<$/>>, <<$;>>], [global]),
    {Type, Subtype}.

transform_headers_to_email(undefined) ->
    [];
transform_headers_to_email(Headers) ->
    lists:map(fun
        ({Name, longstr, Value}) -> {Name, Value};
        ({Name, array, List}) -> {Name, lists:foldr(fun
                ({longstr, Value}, undefined) -> Value;
                ({longstr, Value}, Acc) -> <<Value/binary, $;, Acc/binary>>
            end, undefined, List)}
    end, Headers).

% end of file

