-module(hackney_dev).
-compile([export_all]).

call(undefined, Method, Url, Path, OHeaders, Payload, Options) ->
    io:format(user, "~nurl: ~p~n", [Url ++ Path]),
    %io:format(user, "method: ~p~n", [Method]),
    %io:format(user, "headers: ~p~n", [OHeaders]),
    %io:format(user, "body: ~p~n", [Payload]),
    Bin = fun azure_helplib:to_bin/1,
    Headers = [{Bin(Key), Bin(Value)} || {Key, Value} <- OHeaders],
    {ok, Code, AnswerHeaders, Client} = hackney:request(Method, Url ++ Path, Headers, Payload, Options),
    {ok, Body, NewClient} = hackney:body(Client),
    %io:format(user, "code: ~p~n", [Code]),
    %io:format(user, "headers: ~p~n", [AnswerHeaders]),
    %io:format(user, "body: ~p~n", [Body]),
    {Code, AnswerHeaders, Body, NewClient};

call(Client, Method, _Url, Path, Headers, Payload, _Options) ->
    NextReq = {Method, "/" ++ Path, Headers, Payload},
    {ok, Code, AnswerHeaders, Client1} = hackney:send_request(Client, NextReq),
    {ok, Body, Client2} = hackney:body(Client1),
    {Code, AnswerHeaders, Body, Client2}.

