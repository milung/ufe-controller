:- module(server, [
    server/0, 
    server/1                 % +Port:number

]).
%! <module> HTTP server hooks
% Predicates for creating and running http server.
:- use_module(library(execution_context)).
:- use_module(library(openapi)).
:- use_module(library(swagger_ui)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(source(api/api)). % api implementation is assumed to export all operations for openapi package serving


:- multifile 
    user:file_search_path/2,
    http:status_page/3,
    http:location/3.

:- dynamic   
    http:location/3.

% log warning and error debug messages
:- debug(warning).
:- debug(warning(_)).
:- debug(error).
:- debug(error(_)).

:- context_variable(port, number, [
    env('HTTP_PORT'), default(80), 
    describe('HTTP port the server is listening on.')]).
:- context_variable(server_base_url, atom,
    [env('BASE_URL'), default('/'), 
     describe('Base URL of the server, all absolute links are prefixed with this address')]).

% all assets are served with http_reply_file/2
user:file_search_path(assets, './assets').

%%% PUBLIC PREDICATES_SECTION %%%%%%%%%%%%%%

%! server is det
%  Starts http server listening at the default port specified by the server:port settings. 
server :-     
    context_variable_value(port, Port),
    server(Port).

%! server(+Port:number) is det
%  Starts http server listening at the port specified by the argument Port. 
server(Port) :- 
    http_server(dispatch, [port(Port)]).

%%% PRIVATE PREDICATES_SECTION %%%%%%%%%%%%%%

% dispatch(Request) :-
%     openapi_dispatch(Request),
%     !.
dispatch(Request) :-
    http_dispatch(Request).

http:status_page(not_found(URL), _Context, HTML) :-
    phrase(
        page(
            [ title('Sorry, no such page')], 
            {|html(URL) || <h1>Sorry, no such page <span>URL</span></h1>|} ), 
        HTML).

is_server_dead(Port) :- 
    http_current_worker(Port, _ ), 
    ! .   



%! server_start_and_wait is det
%  Starts the http server listening at the port specified by the server:port setting and wait
%  indefinetely untiol is_server_dead/1 is not succeded. 
server_start_and_wait :-
    context_variable_value(port, Port),
    server(Port),
    repeat,
    sleep(10),
    \+ is_server_dead(Port).

%! server_start is det
%  Prints current server settings and  starts the server
server_start :- 
    setting(user:app_name, AppName),
    setting(user:app_version, Version),
    setting(user:app_authority, Authority),
    format('~n, version ~w, by ~w', [AppName, Version, Authority]), nl, nl, 
    server_start_and_wait.


% :- openapi_server('./assets/openapi.yaml', []).