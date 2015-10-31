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
    auth_user,
    sender_pid,
    options = [] :: list() }).

-define(AUTH_REQUIRED, "530 SMTP authentication is required").

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

handle_HELO(Hostname, State) ->
    rabbit_log:info("HELO from ~s~n", [Hostname]),
    case application:get_env(rabbitmq_email, server_auth) of
        {ok, false} ->
            {ok, 655360, State#state{auth_user=anonymous}}; % 640kb should be enough for anyone
        _Else ->
            % we expect EHLO will come
            {ok, State} % use the default 10mb limit
    end.

handle_EHLO(Hostname, Extensions, State) ->
    rabbit_log:info("EHLO from ~s~n", [Hostname]),
    {ok, auth_extensions(starttls_extension(Extensions)), State}.

auth_extensions(Extensions) ->
    case application:get_env(rabbitmq_email, server_auth) of
        {ok, false} -> Extensions;
        {ok, rabbitmq} -> [{"AUTH", "PLAIN LOGIN"} | Extensions]
    end.

starttls_extension(Extensions) ->
    case application:get_env(rabbitmq_email, server_starttls) of
        {ok, false} -> Extensions;
        {ok, true} -> [{"STARTTLS", true} | Extensions]
    end.

handle_MAIL(_From, State=#state{auth_user=undefined}) ->
    {error, ?AUTH_REQUIRED, State};
handle_MAIL(_From, State) ->
    % you can accept or reject the FROM address here
    {ok, State}.

handle_MAIL_extension(Extension, _State) ->
    rabbit_log:warning("Unknown MAIL FROM extension ~s~n", [Extension]),
    error.

handle_RCPT(_From, State=#state{auth_user=undefined}) ->
    {error, ?AUTH_REQUIRED, State};
handle_RCPT(_To, State) ->
    % you can accept or reject RCPT TO addesses here, one per call
    {ok, State}.

handle_RCPT_extension(Extension, _State) ->
    rabbit_log:warning("Unknown RCPT TO extension ~s~n", [Extension]),
    error.

handle_DATA(_From, _To, _Data, #state{auth_user=undefined} = State) ->
    {error, ?AUTH_REQUIRED, State};
handle_DATA(_From, _To, <<>>, State) ->
    {error, "552 Message too small", State};
handle_DATA(From, To, Data, State=#state{sender_pid=SenderPid}) ->
    % some kind of unique id
    Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(os:timestamp()))]),
    % log for debugging purposes
    case application:get_env(rabbitmq_email, email_store) of
	undefined -> ok;
	{ok, Dir} -> file:write_file(Dir++"/mail-"++Reference++".txt", Data)
    end,

    case rabbit_email_filter:extract_payload(Data) of
        {ok, ContentType, Headers, Body} ->
            rabbit_log:info("~s message from ~s to ~p queued as ~s~n", [ContentType, From, To, Reference]),
            gen_server:cast(SenderPid, {Reference, To, ContentType, Headers, Body}),
            {ok, Reference, State};
        false ->
            rabbit_log:error("message from ~s to ~p cannot be delivered~n", [From, To]),
            {error, "554 Message cannot be delivered", State}
    end.

handle_RSET(State) ->
    % reset any relevant internal state
    State.

handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.

handle_other(Verb, _Args, State) ->
    % You can implement other SMTP verbs here, if you need to
    {["500 Error: command not recognized : '", Verb, "'"], State}.

handle_AUTH(Type, Username, Password, State) when Type =:= login; Type =:= plain ->
    case application:get_env(rabbitmq_email, server_auth) of
        {ok, rabbitmq} ->
            case rabbit_access_control:check_user_pass_login(Username, Password) of
                {ok, AuthUser} ->
                    {ok, State#state{auth_user=AuthUser}};
                {refused, _U, F, A} ->
                    rabbit_log:error(F, A),
                    error
            end;
        {ok, false} ->
            % authentication is disabled; whatever you send is fine
            {ok, State#state{auth_user=anonymous}}
    end;
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


% end of file

