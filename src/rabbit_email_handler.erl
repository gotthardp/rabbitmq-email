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
        {true, {T,S,H,A,Body}} ->
            rabbit_log:info("message from ~s to ~p queued as ~s~n", [From, To, Reference]),
            ContentType = <<T/binary, $/, S/binary>>,
            gen_server:cast(SenderPid, {Reference, To, ContentType, Body}),
            % At this point, if we return ok, we've accepted responsibility for the email
            {ok, Reference, State};
        false ->
            rabbit_log:error("message from ~s to ~p cannot be delivered~n", [From, To]),
            {error, "Message cannot be delivered", State}
    end.

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
    % note: decoding requires eiconv
    try mimemail:decode(Data) of
        {T,S,H,A,P} -> filter_body({T,S,H,A,P})
    catch
    What:Why ->
        rabbit_log:error("Message decode FAILED with ~p:~p~n", [What, Why]),
        false
    end;

filter_body({<<"multipart">>, Subtype, Header, Params, Parts}) ->
    % rabbit_log:info("Parsing multipart/~p~n", [Subtype]),
    case filter_body_parts(filter_multipart(Subtype, Parts)) of
        [] ->
            false;
        [Part] ->
            {true, Part};
        Parts2 when is_list(Parts2) ->
            {true, {<<"multipart">>, Subtype, Header, Params, Parts2}}
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
        true ->
            false
    end;

% remove proprietary formats
filter_body({<<"application">>, <<"ms-tnef">>, _H, _A, _P}) -> false;

filter_body({T,S,_H,_A,_P} = Body) ->
    % rabbit_log:info("Parsing ~p/~p~n", [T, S]),
    {true, Body}.

% lists:filtermap replacement for older Erlangs
filter_body_parts(List1) ->
    lists:foldr(fun(Elem, Acc) ->
        case filter_body(Elem) of
            false -> Acc;
            true -> [Elem|Acc];
            {true,Value} -> [Value|Acc]
        end
    end, [], List1).

filter_multipart(<<"alternative">>, Parts) ->
    Best = lists:foldl(
        fun (Body1, undefined) -> Body1;
            (_, {<<"text">>, <<"plain">>, _, _, _}=Body2) -> Body2;
            ({<<"text">>, <<"plain">>, _, _, _}=Body3, _) -> Body3;
            (_, {<<"text">>, _, _, _, _}=Body4) -> Body4;
            ({<<"text">>, _, _, _, _}=Body5, _) -> Body5;
            (_, Else) -> Else
        end, undefined, Parts),
    [Best];

filter_multipart(<<"mixed">>, Parts) ->
    Parts.

% end of file

