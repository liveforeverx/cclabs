-module(azure_helplib).
-compile([export_all]).

create_table(Name) ->
 "<?xml version=\"1.0\" encoding=\"utf-8\" standalone=\"yes\"?>
  <entry xmlns:d=\"http://schemas.microsoft.com/ado/2007/08/dataservices\"
    xmlns:m=\"http://schemas.microsoft.com/ado/2007/08/dataservices/metadata\"
    xmlns=\"http://www.w3.org/2005/Atom\">
    <title />
    <updated>2009-03-18T11:48:34.9840639-07:00</updated>
    <author>
      <name/>
    </author>
      <id/>
      <content type=\"application/xml\">
        <m:properties>
          <d:TableName>" ++ Name ++ "</d:TableName>
        </m:properties>
      </content>
    </entry>".

create_message(Message) ->
    "<QueueMessage>
     <MessageText>" ++ Message ++ "</MessageText>
     </QueueMessage>".

simple_id() ->
    <<Id:8/binary, _/binary>> = uuid_hex(),
    Id.

uuid_hex() ->
    % create uuid binary
    R1 = crypto:rand_uniform(0, 281474976710656),
    R2 = crypto:rand_uniform(0, 4096),
    R3 = crypto:rand_uniform(0, 4294967296),
    R4 = crypto:rand_uniform(0, 1073741824),
    UUIDBin = <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>,

    % extract numbers for display
    <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>> = UUIDBin,
    list_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
                                 [TL, TM, THV, CSR, CSL, N])).

to_bin(Msg) when is_integer(Msg) ->
    to_bin(integer_to_list(Msg));
to_bin(Msg) when is_atom(Msg) ->
    to_bin(atom_to_list(Msg));
to_bin(Msg) when is_list(Msg) ->
    list_to_binary(Msg);
to_bin(Msg) ->
    Msg.

timestamp() ->
    encode_timestamp(erlang:universaltime()).

encode_timestamp({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    list_to_binary(io_lib:fwrite("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [Year, Month, Day, Hour, Minute, Second])).

create_container(ContainerName, IsPublic) ->
    CustomHeaders = case IsPublic of
                        true -> [{"x-ms-prop-publicaccess", "true"}];
                        _    -> []
                    end,
    do_blob(ContainerName, "", "", "PUT", CustomHeaders, "", "").

delete_container(ContainerName) ->
    do_blob(ContainerName, "", "", "DELETE", [], "", "").

put_blob(ContainerName, BlobName, Data, ContentType)->
    do_blob(ContainerName, BlobName, "", "PUT", [{"x-ms-blob-type", "BlockBlob"}], ContentType, Data).

put_block(ContainerName, BlobName, BlockId, Data, ContentType)->
    do_blob(ContainerName, BlobName, ["comp=block", "blockid=" ++ BlockId], "PUT", [], ContentType, Data).

put_block_list(ContainerName, BlobName, Data, ContentType)->
    do_blob(ContainerName, BlobName, ["comp=blocklist"], "PUT", [], ContentType, Data).

get_blob(ContainerName, BlobName, Start, End) ->
    SStrart = integer_to_list(Start),
    SEnd = integer_to_list(End),
    Headers = [{"Range", "bytes=" ++ SStrart ++ "-" ++ SEnd}],
    io:format(user, "headers: ~p~n", [Headers]),
    do_blob(ContainerName, BlobName, "", "GET", Headers, "", "").

delete_bloblob(ContainerName, BlobName) ->
    do_blob(ContainerName, BlobName, "", "DELETE", [], "", "").

create_queue(Name) ->
    do_queue(Name, "", "", "PUT", [], "", "").

get_mes(Name) ->
    do_queue(Name, "messages", ["numofmessages=1"], "GET", [], "", "").

put_mes(Name, Message) ->
    do_queue(Name, "messages", ["messagettl=1200"], "POST", [], "", create_message(Message)).

do_queue(ContainerName, ResourcePath, AddPath, HttpMethod, CustomHeaders, ContentType, Data) ->
    do_request("queue", ContainerName, ResourcePath, AddPath, HttpMethod, CustomHeaders, ContentType, Data).
do_blob(ContainerName, ResourcePath, AddPath, HttpMethod, CustomHeaders, ContentType, Data) ->
    do_request("blob", ContainerName, ResourcePath, AddPath, HttpMethod, CustomHeaders, ContentType, Data).

do_request(DbType, ContainerName, ResourcePath, AddPath, HttpMethod, CustomHeaders, ContentType, Data)->
    {ok, Account} = application:get_env('ccl5', account),
    {ok, Key} = application:get_env('ccl5', azure_key),
    do_request(DbType, Account, Key, ContainerName, ResourcePath, AddPath, HttpMethod, CustomHeaders, ContentType, Data).

do_request(DbType, Account, Key, ContainerName, ResourcePath, AddPath, HttpMethod, CustomHeaders, ContentType, Data)->
    Date = httpd_util:rfc1123_date(),
    HeadersToSign =[{"x-ms-date", Date},
                    {"x-ms-version", "2011-08-18"},
                    {"Content-Type", ContentType} | CustomHeaders],
    case ResourcePath of
        [] -> ResourcePathToConcat = "";
        _  -> ResourcePathToConcat = "/" ++ ResourcePath
    end,
    Path = "/" ++ Account ++ "/" ++ ContainerName ++ ResourcePathToConcat,
    ContentLength = get_length(Data),
    Authorization = get_auth_header(Path, AddPath, HttpMethod, HeadersToSign, Key,  ContentType, ContentLength),
    Headers = [{"Authorization", "SharedKey " ++ Account ++ ":" ++ Authorization},
               {"Content-Length", integer_to_list(ContentLength)} | HeadersToSign],
    Options = [],
    AddPathString = case AddPath of
                        "" ->
                            "";
                        [Add | AddPathTail] ->
                            lists:foldl(fun(AddP, Acc) ->
                                                Acc ++ "&" ++ AddP
                                        end, "?" ++ Add, AddPathTail)
                    end,
    Url = "http://" ++ Account ++ "." ++ DbType ++ ".core.windows.net/" ++ ContainerName ++
          ResourcePathToConcat ++ AddPathString,
    Method = list_to_atom(string:to_lower(HttpMethod)),
    hackney_dev:call(undefined, Method, Url, "", Headers, Data, Options).

get_auth_header(Path, AddPath, HttpMethod, HeadersToSign, Key, ContentType, ContentLength) ->
    MS_Header_Keys = [HeaderKey || {HeaderKey, _} <- HeadersToSign, lists:prefix("x-ms", HeaderKey)],
    Sorted_MS_Header_Keys = lists:sort(MS_Header_Keys),
    CanonicalHeaderFun = fun( HeaderKey, AccIn) ->
                 {value, {HeaderKey, HeaderValue}} = lists:keysearch(HeaderKey, 1, HeadersToSign),
                 AccIn ++ HeaderKey ++ ":" ++ HeaderValue ++ "\n" end,
    CanonicalHeaders = lists:foldl(CanonicalHeaderFun , "", Sorted_MS_Header_Keys),
    ResourceFun = fun(V) ->
                          {_, [A, B]} = re:run(V, "^(.*?)[=](.*)$", [{capture, all_but_first, list}]),
                          "\n" ++ A ++ ":" ++ B
                  end,
    CanonicalResource = Path ++ lists:flatmap(ResourceFun, lists:sort(AddPath)),
    Length = integer_to_list(ContentLength),
    Range = proplists:get_value("Range", HeadersToSign, ""),
    StringToSign = HttpMethod ++ "\n\n\n" ++ Length ++ "\n\n" ++ ContentType ++
                   "\n\n\n\n\n\n" ++ Range ++ "\n" ++ CanonicalHeaders ++ CanonicalResource,
    DecodedKey = base64:decode(Key),
    binary_to_list( base64:encode( hmac256:digest(binary_to_list(DecodedKey), StringToSign))).

get_length(Data) when is_list(Data) -> length(Data);
get_length(Data) when is_binary(Data) -> size(Data).
