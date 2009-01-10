%% Copyright (c) 2009 Nick Gerakines <nick@gerakines.net>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% 
%% Change Log:
%% * v0.1 2009-01-09: ngerakines
%%   - Initial release
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @copyright 2009 Nick Gerakines
%% @version 0.1
%% @doc An Erlang client to interface with the LighthouseApp API.
%% 
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/ngerakines/erlang_lighthouse/
-module(lighthouse).
-author("Nick Gerakines <nick@gerakines.net>").
-version("Version: 0.1").

-export([projects/2, create_ticket/4, create_ticket/5, tickets/3, tickets/4]).

-include_lib("xmerl/include/xmerl.hrl").

-define(ATTRIBUTE_INT, #xmlAttribute{name = 'type', value = "integer"}).

%% @private
raw_request(Type, Server, Auth, URI, Body) ->
    {ok, Socket} = gen_tcp:connect(
        Server ++ ".lighthouseapp.com",
        80,
        [binary, {active, false}, {packet, 0}]
    ),
    Req = build_request(Type, Server ++ ".lighthouseapp.com", URI, Auth, Body),
    io:format("Req: ~p~n", [Req]),
    gen_tcp:send(Socket, Req),
    {ok, Resp} = do_recv(Socket, []),
    io:format("Resp: ~p~n", [Resp]),
    gen_tcp:close(Socket),
    {ok, _, ResponseBody} = erlang:decode_packet(http, Resp, []),
    decode_xml(parse_response(ResponseBody)).

%% @private
do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} -> do_recv(Sock, [Bs | B]);
        {error, _} -> {ok, erlang:iolist_to_binary(Bs)}
    end.

%% @private
build_request(Type, Host, URI, Auth, Body) ->
    AuthHeader = build_headers(Auth),
    erlang:iolist_to_binary([
        Type, " ", URI, " HTTP/1.0\r\n",
        "User-Agent: erlang_lighthouse/0.1\r\n",
        "Host: ", Host, "\r\n",
        AuthHeader,
        "Content-Length: ", integer_to_list(erlang:iolist_size(Body)), "\r\n",
        "Content-Type: application/xml\r\n\r\n",
        Body
    ]).

%% @private
build_headers({APIKey}) ->
    build_headers({APIKey, "x"});
build_headers({Username, Password}) ->
    erlang:iolist_to_binary([
        "Authorization: Basic ", base64:encode(Username ++ ":" ++ Password), "\r\n"
    ]).

%% @private
value_or_default(Key, List, Default) ->
    proplists:get_value(Key, List, Default).

%% @private
build_querystring(Args) -> 
    lists:concat(["?", mochiweb_util:urlencode(Args)]).

%% @private
parse_response(<<13,10,13,10,Data/binary>>) -> binary_to_list(Data);
parse_response(<<_X:1/binary,Data/binary>>) -> parse_response(Data).

%% @private
decode_xml(Body) ->
    try xmerl_scan:string(Body) of
        {Xml, _} -> simple_xml(Xml);
        Other -> {other, Other}
    catch
        _:_ -> {raw, Body}
    end.

%% @private
simple_xml(#xmlElement{name = Name, attributes = Attrs, content = Content}) ->
    Elements = lists:foldr(
        fun(E, L) -> case simple_xml(E) of nothing -> L; V -> [V | L] end end,
        [],
        Content
    ),
    Attribs = [{K, V} || #xmlAttribute{name = K, value = V} <- Attrs],
    {Name, Attribs, Elements};
simple_xml(#xmlText{value = Value}) ->
    case lists:all(
        fun (10) -> true;
            (13) -> true;
            (32) -> true;
            (9) -> true;
            (_) -> false
        end,
        Value
    ) of
        true -> nothing;
        _ -> Value
    end.

%% @spec projects(Server, Authentication) -> Result
%%       Server = string()
%%       Authentication = {string()} | {string(), string()}
%%       Result = any()
%% @doc Retrieve the projects of an account.
projects(Server, Authentication) ->
    Url = "http://" ++ Server ++ ".lighthouseapp.com/projects.xml",
    raw_request("GET", Server, Authentication, Url, []).

%% @equiv create_ticket(Server, Authentication, Project, Title, [])
create_ticket(Server, Authentication, Project, Title) ->
    create_ticket(Server, Authentication, Project, Title, []).

%% @equiv tickets(Server, Authentication, Project, [])
tickets(Server, Authentication, Project) ->
    tickets(Server, Authentication, Project, []).

%% @spec tickets(Server, Authentication, Project, Options) -> Result
%%       Server = string()
%%       Authentication = {string()} | {string(), string()}
%%       Project = string()
%%       Options = [Option]
%%       Option = {"q", string()} | {"page", integer()}
%%       Result = any()
%% @doc Retrieve one or more tickets for a given project. Using the "q"
%% option it is possible to use search strings.
tickets(Server, Authentication, Project, Options) ->
    Url = lists:concat([
        "http://", Server, ".lighthouseapp.com",
        "/projects/", Project, "/tickets.xml",
        build_querystring(Options)
    ]),
    raw_request("GET", Server, Authentication, Url, []).

%% @spec create_ticket(Server, Authentication, Project, Title, Options) -> Result
%%       Server = string()
%%       Authentication = {string()} | {string(), string()}
%%       Project = string()
%%       Title = string()
%%       Options = [{Option, string()}]
%%       Option = user | milestone | state | body
%%       Result = any()
%% @doc Create a ticket in a project.
create_ticket(Server, Authentication, Project, Title, Options) ->
    Data = [
        {'assigned-user-id', [?ATTRIBUTE_INT], [value_or_default('user', Options, "")]},
        {'milestone-id', [?ATTRIBUTE_INT], [value_or_default('milestone', Options, "")]},
        {state, [], [value_or_default('state', Options, "")]},
        {title, [], [Title]},
        {body, [], [value_or_default('body', Options, "")]}
    ],
    {RootEl, _} = xmerl_scan:string("<ticket />"),
    #xmlElement{content = Content} = RootEl,
    NewContent = Content ++ lists:flatten([Data]),
    NewRootEl = RootEl#xmlElement{content=NewContent},    
    Export = xmerl:export_simple([NewRootEl], xmerl_xml),
    Url = lists:concat([
        "http://", Server, ".lighthouseapp.com",
        "/projects/", Project, "/tickets.xml"
    ]),
    raw_request("POST", Server, Authentication, Url, lists:flatten(Export)).
