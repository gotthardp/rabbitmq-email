%
% This Source Code Form is subject to the terms of the Mozilla Public
% License, v. 2.0. If a copy of the MPL was not distributed with this
% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%
% Copyright (C) 2014-2015 Petr Gotthard <petr.gotthard@centrum.cz>
%

-module(rabbit_email_filter).
-export([extract_payload/1]).

extract_payload(Data) ->
    case application:get_env(rabbitmq_email, email_filter) of
        % filtering is disable, just pass on the entire e-mail
        {ok, false} ->
            {ok, <<"application/mime">>, [], Data};
        % extract the useful content
        _Else ->
            case filter_body(Data) of
                {send, {Type,Subtype,Headers,Params,Body}} when is_binary(Body) ->
                    {ok, <<Type/binary, $/, Subtype/binary>>, extract_headers(Headers, Params), Body};
                {send, Multipart} ->
                    {ok, <<"application/mime">>, [], mimemail:encode(Multipart)};
                {empty, {_,_,Headers,Params,_}} ->
                    {ok, <<>>, extract_headers(Headers, Params), <<>>};
                drop ->
                    error
            end
    end.

extract_headers(Headers, Params) ->
        ContentTypeParams = proplists:get_value(<<"content-type-params">>, Params, []),
        AllHeaders = lists:merge(Headers, ContentTypeParams),
        lists:filter(fun filter_header/1, AllHeaders).

filter_header({Name, _Value}) ->
    Name2 = string:to_lower(binary_to_list(Name)),
    {ok, Filter} = application:get_env(rabbitmq_email, email_headers),
    lists:member(Name2, Filter).

filter_body(Data) when is_binary(Data) ->
    try mimemail:decode(Data) of
        {T,S,H,A,P} -> filter_body({T,S,H,A,P})
    catch
    What:Why ->
        rabbit_log:error("Message decode FAILED with ~p:~p~n", [What, Why]),
        drop
    end;

filter_body({<<"multipart">>, Subtype, Header, _Params, Parts}=Parent) ->
    % rabbit_log:info("Parsing multipart/~p~n", [Subtype]),
    % pass 1: filter undersirable content
    case filter_multipart(Subtype, Parts) of
        [] ->
            drop;
        [{Type2, Subtype2, _Header2, Params2, Parts2}] ->
            % keep the top-most headers
            % FIXME: some top-most should be preserved, but not Content-Type
            {send, {Type2, Subtype2, Header, Params2, Parts2}};
        Parts3 when is_list(Parts3) ->
            % pass 2: select the best part
            {ok, Filter} = application:get_env(rabbitmq_email, email_filter),
            {send, best_multipart(Parts3, lists:reverse(Filter), Parent)}
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
            {send, {<<"text">>, Subtype, Header, Params, Text3}};
        Subtype == <<"plain">> ->
            {empty, {<<"text">>, Subtype, Header, Params, <<>>}};
        true ->
            drop
    end;

% remove proprietary formats
filter_body({<<"application">>, <<"ms-tnef">>, _H, _A, _P}) -> drop;
% and accept the rest
filter_body(Body) -> {send, Body}.

% when text/plain in multipart/alternative is empty, the entire body is empty
filter_multipart(<<"alternative">>, List) ->
    case filter_bodies(List) of
        {_, true} -> [];
        {Acc, false} -> Acc
    end;

filter_multipart(_, List) ->
    {Acc, _} = filter_bodies(List),
    Acc.

filter_bodies(List1) ->
    lists:foldr(
        fun (Elem, {Acc, WasEmpty}) ->
            case filter_body(Elem) of
                {send,Value} -> {[Value|Acc], WasEmpty};
                {empty,_} -> {Acc, true};
                drop -> {Acc, WasEmpty}
            end
        end, {[], false}, List1).

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

% end of file

