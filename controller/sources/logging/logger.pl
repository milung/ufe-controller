:- module(logger, [
    log/5,             %  +Level:atom, +Module:atom, +MessageFmt:string, +FormatArgs:list, +Options:list
    logged_http/2      % :Goal, +Request:list
]).
%! <module> logger doing some cool things
%  Predicates for logger ...



:- multifile
  prolog:message//1,
  http:status_page_hook/3.

:- meta_predicate logged_http(1, +).
:- use_module(library(http/html_write)).
:- use_module(library(http/json)).

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
    call(Goal, Request),
    !.
% only in error case when goal have failed.
 logged_http(Goal, Request) :-  
    (   Goal =.. [':', Module, ModuleGoal]
    ->  functor(ModuleGoal, Func, Arity)
    ;   Module = user,
        functor(Goal, Func, Arity)
    ),
    Arity1 is Arity + 1,
    print_message(error, http_request(goal_failed, Request, Module:Func/Arity1)),
    throw(http_reply(server_error(goal_failure(Module:Func/Arity1)))).


log(Level, Module, MessageFmt, FmtArgs, _) :-
    format(string(Message), MessageFmt, FmtArgs),
    print_message(informational, json_log(_{ level: Level, message: Message, module: Module })).

%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

prolog:message(json_log(Data)) -->
    { atom_json_dict( JSON, Data, [as(string), width(0)]) },
    ["~w" - [JSON] ].
prolog:message(http_request(received, Request, Module:Functor/Arity)) -->
    { memberchk(request_uri(Uri), Request) },
    ["HTTP request for ~p dispatched to ~w:~w/~w" - [Uri, Module, Functor, Arity] ].

prolog:message(http_request(goal_failed, Request, Module:Functor/Arity)) -->
    { memberchk(request_uri(Uri), Request) },
    ["[500] Goal failed while serving HTTP request for ~p dispatched to ~w:~w/~w" - [Uri, Module, Functor, Arity] ].

% http:status_page_hook(server_error(goal_failure(Goal)), _Context, HTML) :-
%     phrase( page([ title('500 Internal Server Error')
%                 ],
%                 [ h1('Internal Server Error'),
%                 p(['The implementation of the procedures for handling of the request failed, the issue seems to be on the implementation side of the server'
%                     ])
%                 ]),
%         HTML).