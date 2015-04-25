%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014 Petr Gotthard <petr.gotthard@centrum.cz>
%
% Based on smtp_server_example
% Copyright 2009-2011 Andrew Thompson <andrew@hijacked.us>
%

-module(rabbit_email_handler).
-behaviour(gen_smtp_server_session).

-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
    handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
    handle_other/3, handle_AUTH/4, handle_STARTTLS/1, handle_info/2,
    code_change/3, terminate/2]).

-record(state, {
    sender_pid,
    options = [] :: list() }).

init(Hostname, SessionCount, Address, Options) when SessionCount < 20 ->
    rabbit_log:info("~s SMTP connection from ~p~n", [Hostname, Address]),
    process_flag(trap_exit, true),
    {ok, SenderPid} = rabbit_message_sender:start_link(Hostname),

    Banner = [Hostname, " ESMTP rabbit_email_handler"],
    State = #state{sender_pid=SenderPid, options=Options},
    {ok, Banner, State};

init(Hostname, _SessionCount, _Address, _Options) ->
    rabbit_log:warning("Connection limit exceeded~n"),
    {stop, normal, ["421 ", Hostname, " is too busy to accept mail right now"]}.

handle_HELO(<<"invalid">>, State) ->
    % contrived example
    {error, "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
    {ok, State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
    rabbit_log:info("HELO from ~s~n", [Hostname]),
    {ok, 655360, State}. % 640kb of HELO should be enough for anyone.
    %If {ok, State} was returned here, we'd use the default 10mb limit

handle_EHLO(<<"invalid">>, _Extensions, State) ->
    % contrived example
    {error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
    rabbit_log:info("EHLO from ~s~n", [Hostname]),
    % You can advertise additional extensions, or remove some defaults
    MyExtensions = case proplists:get_value(auth, State#state.options, false) of
        true ->
            % auth is enabled, so advertise it
            Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
        false ->
            Extensions
    end,
    {ok, MyExtensions, State}.

handle_MAIL(<<"badguy@blacklist.com">>, State) ->
    {error, "552 go away", State};
handle_MAIL(_From, State) ->
    % you can accept or reject the FROM address here
    {ok, State}.

handle_MAIL_extension(<<"X-SomeExtension">> = Extension, State) ->
    rabbit_log:info("Mail from extension ~s~n", [Extension]),
    % any MAIL extensions can be handled here
    {ok, State};
handle_MAIL_extension(Extension, _State) ->
    rabbit_log:warning("Unknown MAIL FROM extension ~s~n", [Extension]),
    error.

handle_RCPT(<<"nobody@example.com">>, State) ->
    {error, "550 No such recipient", State};
handle_RCPT(_To, State) ->
    % you can accept or reject RCPT TO addesses here, one per call
    {ok, State}.

handle_RCPT_extension(<<"X-SomeExtension">> = Extension, State) ->
    % any RCPT TO extensions can be handled here
    rabbit_log:info("Mail to extension ~s~n", [Extension]),
    {ok, State};
handle_RCPT_extension(Extension, _State) ->
    rabbit_log:warning("Unknown RCPT TO extension ~s~n", [Extension]),
    error.

handle_DATA(_From, _To, <<>>, State) ->
    {error, "552 Message too small", State};
handle_DATA(From, To, Data, #state{sender_pid=SenderPid} = State) ->
    % some kind of unique id
    Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),

    case filter_body(Data) of
        {true, {Type,Subtype,Headers,Params,Body}} when is_binary(Body) ->
            rabbit_log:info("~s/~s message from ~s to ~p queued as ~s~n", [Type, Subtype, From, To, Reference]),
            ContentType = <<Type/binary, $/, Subtype/binary>>,
            Headers2 = extract_headers(Headers, Params),
            gen_server:cast(SenderPid, {Reference, To, ContentType, Headers2, Body}),
            % At this point, if we return ok, we've accepted responsibility for the email
            {ok, Reference, State};
        {true, Multipart} ->
            rabbit_log:info("application/mime message from ~s to ~p queued as ~s~n", [From, To, Reference]),
            Body2 = mimemail:encode(Multipart),
            gen_server:cast(SenderPid, {Reference, To, <<"application/mime">>, [], Body2}),
            {ok, Reference, State};
        {empty, {_,_,Headers,Params,_}} ->
            rabbit_log:info("empty message from ~s to ~p queued as ~s~n", [From, To, Reference]),
            Headers2 = extract_headers(Headers, Params),
            gen_server:cast(SenderPid, {Reference, To, <<>>, Headers2, <<>>}),
            {ok, Reference, State};
        false ->
            rabbit_log:error("message from ~s to ~p cannot be delivered~n", [From, To]),
            {error, "554 Message cannot be delivered", State}
    end.

extract_headers(Headers, Params) ->
        ContentTypeParams = proplists:get_value(<<"content-type-params">>, Params, []),
        AllHeaders = lists:merge(Headers, ContentTypeParams),
        lists:filter(fun filter_header/1, AllHeaders).

handle_RSET(State) ->
    % reset any relevant internal state
    State.

handle_VRFY(<<"someuser">>, State) ->
    {ok, "someuser@"++smtp_util:guess_FQDN(), State};
handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.

handle_other(Verb, _Args, State) ->
    % You can implement other SMTP verbs here, if you need to
    {["500 Error: command not recognized : '", Verb, "'"], State}.

handle_AUTH(Type, <<"username">>, <<"PaSSw0rd">>, State) when Type =:= login; Type =:= plain ->
    {ok, State};
handle_AUTH('cram-md5', <<"username">>, {Digest, Seed}, State) ->
    case smtp_util:compute_cram_digest(<<"PaSSw0rd">>, Seed) of
        Digest -> {ok, State};
        _ -> error
    end;
handle_AUTH(_Type, _Username, _Password, _State) ->
    error.

handle_STARTTLS(State) ->
    rabbit_log:info("TLS Started~n"),
    State.

handle_info({'EXIT', SenderPid, _Reason}, #state{sender_pid=SenderPid} = State) ->
    % sender failed, we terminate as well
    {stop, normal, State};

handle_info(Info, State) ->
    rabbit_log:info("~w~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, #state{sender_pid=SenderPid} = State) ->
    gen_server:cast(SenderPid, stop),
    {ok, Reason, State}.

%% body decoding functions

filter_body(Data) when is_binary(Data) ->
    try mimemail:decode(Data) of
        {T,S,H,A,P} -> filter_body({T,S,H,A,P})
    catch
    What:Why ->
        rabbit_log:error("Message decode FAILED with ~p:~p~n", [What, Why]),
        false
    end;

filter_body({<<"multipart">>, Subtype, Header, _Params, Parts}=Parent) ->
    % rabbit_log:info("Parsing multipart/~p~n", [Subtype]),
    % pass 1: filter undersirable content
    case filter_multipart(Subtype, Parts) of
        [] ->
            false;
        [{Type2, Subtype2, _Header2, Params2, Parts2}] ->
            % keep the top-most headers
            % FIXME: some top-most should be preserved, but not Content-Type
            {true, {Type2, Subtype2, Header, Params2, Parts2}};
        Parts3 when is_list(Parts3) ->
            % pass 2: select the best part
            {ok, Filter} = application:get_env(rabbitmq_email, email_filter),
            {true, best_multipart(Parts3, lists:reverse(Filter), Parent)}
    end;

filter_body({<<"text">>, Subtype, Header, Params, Text}) ->
    % remove leading and trailing whitespace
    Text2 = re:replace(Text, "(^\\s+)|(\\s+$)", "", [global, {return, binary}]),
    % convert DOS to Unix EOL
    Text3 = binary:replace(Text2, <<16#0D, 16#0A>>, <<16#0A>>, [global]),
    % do not send empty body
    if
        byte_size(Text3) > 0 ->
            % rabbit_log:info("Parsing text/~p~n", [Subtype]),
            {true, {<<"text">>, Subtype, Header, Params, Text3}};
        Subtype == <<"plain">> ->
            {empty, {<<"text">>, Subtype, Header, Params, <<>>}};
        true ->
            false
    end;

% remove proprietary formats
filter_body({<<"application">>, <<"ms-tnef">>, _H, _A, _P}) -> false;
% and accept the rest
filter_body(Body) -> {true, Body}.

% when text/plain in multipart/alternative is empty, the entire body is empty
filter_multipart(<<"alternative">>, List) ->
    case filter_bodies(List) of
        {true, _} -> [];
        {false, Acc} -> Acc
    end;

filter_multipart(_, List) ->
    {_, Acc} = filter_bodies(List),
    Acc.

filter_bodies(List1) ->
    lists:foldr(fun(Elem, {false, Acc}) ->
        case filter_body(Elem) of
            false -> {false, Acc};
            {empty,_} -> {true, Acc};
            {true,Value} -> {false, [Value|Acc]}
        end
    end, {false, []}, List1).

best_multipart(Parts, [{Type, SubType} | OtherPrios], BestSoFar) when is_binary(Type), is_binary(SubType) ->
    Better = lists:foldl(
        fun (Body1, undefined) -> Body1;
            (_, {T2, S2, _, _, _}=Body2) when T2==Type, S2==SubType -> Body2;
            ({T3, S3, _, _, _}=Body3, _) when T3==Type, S3==SubType -> Body3;
            (_, {T4, _, _, _, _}=Body4) when T4==Type -> Body4;
            ({T5, _, _, _, _}=Body5, _) when T5==Type -> Body5;
            (_, Else) -> Else
        end, BestSoFar, Parts),
    best_multipart(Parts, OtherPrios, Better);

best_multipart(Parts, [{Type, undefined} | OtherPrios], BestSoFar) when is_binary(Type) ->
    Better = lists:foldl(
        fun (Body1, undefined) -> Body1;
            (_, {T2, _, _, _, _}=Body2) when T2==Type -> Body2;
            ({T3, _, _, _, _}=Body3, _) when T3==Type -> Body3;
            (_, Else) -> Else
        end, BestSoFar, Parts),
    best_multipart(Parts, OtherPrios, Better);

best_multipart(Parts, [{undefined, undefined} | OtherPrios], BestSoFar) ->
    Better = lists:foldl(
        fun (Body1, undefined) -> Body1;
            (_, Else) -> Else
        end, BestSoFar, Parts),
    best_multipart(Parts, OtherPrios, Better);

best_multipart(Parts, [], BestSoFar) ->
    lists:foldl(
        fun (Body1, undefined) -> Body1;
            (_, Else) -> Else
        end, BestSoFar, Parts).

filter_header({Name, _Value}) ->
    Name2 = string:to_lower(binary_to_list(Name)),
    {ok, Filter} = application:get_env(rabbitmq_email, email_headers),
    lists:member(Name2, Filter).

% end of file

