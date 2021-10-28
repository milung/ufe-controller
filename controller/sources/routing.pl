:- module(routing, []).
%! <module> HTTP routing 
%  Predicates for basic routing of http requests

:- encoding(utf8).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_header)).
:- use_module(library(execution_context)).
:- use_module(library(mustache)).

:- use_module(source(fe_config/fe_config)).

:- multifile 
    user:file_search_path/2,
    http:status_page/3,
    http:location/3.

:- dynamic   
    http:location/3.


% default rooting - adapt to project needs

:- http_handler(root('fe-config'), serve_fe_config, [prefix]).
:- http_handler(root('assets'), serve_assets, [prefix]).
:- http_handler(root(modules), serve_assets, [prefix]).
:- http_handler(root('web-components'), serve_webcomponents, [prefix]). 
:- http_handler(root('favicon.ico'), http_reply_file(assets('icon/favicon.ico'), []), []).
:- http_handler(root(.), serve_spa, [prefix]).    


%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

%  serve_assets(+Request:list) is det
%  checks for existence of the resource from Request's URI 
%  and serves it to http server. Throws =|http_reply(not_found(Path)|= 
% if asset does not exists
serve_assets( Request) :-
    option(path_info(Asset), Request),
    absolute_file_name(asset(Asset), Absolute),
    exists_file(Absolute),    
    access_file(Absolute, read),
    http_reply_file(asset(Asset), [], Request).
serve_assets(Request) :-
    option(path(Path), Request),
    throw(http_reply(not_found(Path))).


serve_spa( Request) :-
    http_reply_file(html('index.html'), [], Request).