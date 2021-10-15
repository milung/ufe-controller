%% start your debugging session by loading 'debug' file. 

:- [load].

% debug settings to make debugging more fancy
:- use_module(library('http/http_error')).
:- use_module(library('semweb/rdf_portray.pl')).

:- set_prolog_flag(history, 50).
:- load_test_files([]).

% generic debugging
:- debug(trace).
:- debug(trace(_)).
:- debug(debug).
:- debug(debug(_)).
:- debug(info).
:- debug(info(_)).
:- debug(warning).
:- debug(warning(_)).
:- debug(error).
:- debug(error(_)).

% utility scripts for openapi server - comment if you are not using openapi
:- use_module(library(openapi)).

generate_api :-
    openapi_doc(assets('openapi.yaml'), server, [file('sources/api/generated.pl')] ).