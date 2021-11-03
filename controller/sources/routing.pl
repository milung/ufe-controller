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
:- use_module(source(http_extra/http_extra)).

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
:- http_handler(root('favicon.ico'), http_reply_file(asset('icon/favicon.ico'), [headers([cache_control('public, max-age=31536000, immutable')]), cached_gzip(true)]), []).
:- http_handler(root(.), serve_spa, [prefix]).    


%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%  PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%




serve_spa( Request) :-
    http_reply_file(html('index.html'), [], Request).