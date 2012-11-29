-module(azure).
-compile([export_all]).

-define(CONT, "test").
-define(CONTENT, "application/octet-stream").
-define(TEXTPLAIN, "text/plain").
-define(PART, 1024 * 100).
-define(FILENAME, "apache-maven-3.0.4-bin1.zip").

create() ->
    azure_helplib:create_container(?CONT, true).

delete() ->
    azure_helplib:delete_container(?CONT).

put(Threads) ->
    put_p(Threads, "apache-maven-3.0.4-bin1.zip").

put_p(Threads, FileName) ->
    azure_helplib:put_blob(?CONT, FileName, "", ?CONTENT),
    Result = async(Threads, fun(N) -> put_f(N, Threads, FileName) end, 0),
    io:format(user, "result: ~p~n", [Result]),
    Last = lists:min(Result),
    io:format(user, "last: ~p~n", [Last]),
    Ids = lists:seq(0, Last - 1),
    %io:format(user, "~p~n", [[{'BlockList', [], [{'Latest', [], azure_helplib:to_bin(Id)} || Id <- Ids]}]]),
    Xml = xmlsimple:encode([{'BlockList', [{'Latest', [build_id(Id)]} || Id <- Ids]}]),
    azure_helplib:put_block_list(?CONT, FileName, Xml, ?TEXTPLAIN).

async(Threads, _Fun, Threads) ->
    io:format(user, "start sync: ~p~n", [ok]),
    sync(Threads, []);
async(Threads, Fun, Started) ->
    Self = self(),
    spawn(fun() -> Self ! {sync_p, Fun(Started)} end),
    async(Threads, Fun, Started + 1).

sync(0, Acc) -> Acc;
sync(N, Acc) ->
    receive
        {sync_p, Ready} ->
            io:format(user, "get ready: ~p~n", [ok]),
            sync(N - 1, [Ready | Acc])
    after
        900000000 ->
            async_error
    end.

put_f(Location, Threads, FileName) ->
    {ok, File} = file:open(FileName, [raw, binary]),
    put_t(Location, Threads, File, FileName).
put_t(Location, Threads, File, FileName) ->
    case file:pread(File, Location * ?PART, ?PART) of
        {ok, Data} ->
            {C, _, _, _} = azure_helplib:put_block(?CONT, FileName, build_id(Location), Data, ?CONTENT),
            io:format(user, "location ~p code: ~p~n", [Location, C]),
            put_t(Location + Threads, Threads, File, FileName);
        eof ->
            io:format(user, "get eof : ~p~n", [Location]),
            Location
    end.

partname(FileName, N) -> "temp/" ++ FileName ++ ".part" ++ integer_to_list(N).

get_p(Threads) ->
    get_p(Threads, ?FILENAME).

get_p(Threads, FileName) ->
    file:make_dir("temp"),
    Result = async(Threads, fun(N) -> get_f(N, Threads, FileName) end, 0),
    io:format(user, "result: ~p~n", [Result]),
    Last = lists:min(Result),
    io:format(user, "last: ~p~n", [Last]),
    merge(Last, FileName).

get_f(N, Threads, FileName) ->
    Start = N * ?PART,
    End = Start + ?PART - 1,
    PartName = partname(FileName, N),
    case file:read_file_info(PartName) of
        {error, enoent} ->
            {C, _, Body, _} = azure_helplib:get_blob(?CONT, FileName, Start, End),
            io:format(user, "get c: ~p~n", [C]),
            case C of
                206 ->
                    file:write_file(PartName, Body),
                    get_f(N + Threads, Threads, FileName);
                _ ->
                    io:format(user, "body: ~p~n", [Body]),
                    N
            end;
        Exist ->
            get_f(N + Threads, Threads, FileName)
    end.

build_id(Id) ->
    binary_to_list(base64:encode("part" ++ integer_to_list(Id))).

merge(N, FileName) ->
    {ok, File} = file:open("temp/" ++ FileName, [raw, binary, write]),
    merge(File, N, 0, FileName).

merge(File, N, N, FileName) ->
    file:close(File);
merge(File, N, X, FileName) ->
    {ok, Bin} = file:read_file(partname(FileName, X)),
    file:write(File, Bin),
    merge(File, N, X + 1, FileName).

