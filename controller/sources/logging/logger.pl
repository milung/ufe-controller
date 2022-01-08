:- module(logger, [
    logged_http/2      % :Goal, +Request:list
]).
%! <module> logger doing some cool things
%  Predicates for logger ...



:- multifile
  prolog:message//1.

:- meta_predicate logged_http(1, +).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! logged_http(:Goal, +Request:list)
%  same as call, but prints logs. Intended for logging of http request dispatching. 
logged_http(Goal, Request) :-
    (   Goal =.. [':', Module, ModuleGoal]
    ->  functor(ModuleGoal, Func, Arity)
    ;   Module = user,
        functor(Goal, Func, Arity)
    ),
    Arity1 is Arity + 1,
    print_message(informational, http_request(received, Request, Module:Func/Arity1)),
    call(Goal, Request).

%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

prolog:message(http_request(received, Request, Module:Functor/Arity)) -->
    { memberchk(request_uri(Uri), Request) },
    ["HTTP request for ~p dispatched to ~w:~w/~w" - [Uri, Module, Functor, Arity] ].