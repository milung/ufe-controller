:- module(http_extra, [
    http_response/2,    % +Request:list, +Data
    http_response/3,    % +Request:list, +Data, +HdrExtra:list
    http_response/4,    % +Request:list, +Data, +HdrExtra:list, +Code
    http_response/5    % +Request:list, +Data, +HdrExtra:list, +Context, +Code
    ]).
%! <module> http_extra predicates

:- use_module(library(http/http_header)).

% :- use_module(source(module)).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! http_response(+Request:list, +Data) is det
%! http_response(+Request:list, +Data, +HdrExtra:list) is det
%! http_response(+Request:list, +Data, +HdrExtra:list, +Code) is det
%! http_response(+Request:list, +Data, +HdrExtra:list, +Context, +Code) is det
%  This predicate is intended to simplify debugging of the REST API http handlers
%  If the `Request` contains element `debug(true)` then its behavior is same as of `http_reply/6`
%  otherwise it raises exception of type `http_reply(5)`. That allows for a little bit simpler testing 
%  and debugging of of the handlers.
http_response(Request, What) :-
    http_response(Request, What, [connection(close)], _).
    
http_response(Request, Data, HdrExtra) :-
    http_response(Request, Data, HdrExtra, _Code).

http_response(Request, Data, HdrExtra, Code) :-
    http_response(Request, Data, HdrExtra, [], Code).

http_response(Request, Data, HdrExtra, Context,  Code) :-
    option(debug(true), Request),
    select_option(method(M), Request, Request1, get),
    select_option(connection(C), Request1, Request2, close),
    http_reply(Data, current_output, HdrExtra, Context, [method(M), connection(C) | Request2], Code),
    !.
http_response(_, Data, HdrExtra, _, Code) :-
    throw(http_reply(Data, HdrExtra, Code)).
    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%