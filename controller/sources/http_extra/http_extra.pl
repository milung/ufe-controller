:- module(http_extra, [
    http_response/2,            % +Request:list, +Data
    http_response/3,            % +Request:list, +Data, +HdrExtra:list
    http_response/4,            % +Request:list, +Data, +HdrExtra:list, +Status
    request_accept_languages/3, % +Request:list, -Languages:list(atom), +Options
    request_match_condition/4,  % +Request:list, ?CurrentTag:atom, ?LastModification:float, -WeakMatch:boolean
    request_match_language/3,   % +Request:list, +Supported:list(atom), -Language:atom
    serve_assets/1,             % +Request:list
    serve_health_check/1,       % +Request:list
    set_health_status/4,        % +Status:oneof(healthy, unhealthy), +OperationalParameter:atom, +Message:string, +ValidSeconds:number
    set_healthy_status/3,       %  +OperationalParameter:atom, +Message:string, +ValidSeconds:number
    set_healthy_status/2,       %  +OperationalParameter:atom, +Message:string
    set_healthy_status/1,       %  +OperationalParameter:atom
    set_unhealthy_status/2      %  +OperationalParameter:atom, +Message:string
    ]).
%! <module> http_extra predicates

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(dcg/basics)).

user:file_search_path(http_gzip_cache, asset(cache)).

:- dynamic health_status/1.

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! http_response(+Request:list, +Data) is det
%! http_response(+Request:list, +Data, +HdrExtra:list) is det
%! http_response(+Request:list, +Data, +HdrExtra:list, +Code) is det
%! http_response(+Request:list, +Data, +HdrExtra:list, +Context, +Code) is det
%  This predicate is intended to simplify debugging of the REST API http handlers.
%  It effectively delegates to `http_post_data/3`, but if the `Request` contains 
%  element `debug(true)` then it creates temporary memory stream before outputing 
%  it to the console to deal with stream encoding switch.
http_response(Request, What) :-
    http_response(Request, What, [connection(close)], ok).
    
http_response(Request, Data, HdrExtra) :-
    http_response(Request, Data, HdrExtra, ok).

http_response(Request, Data, HdrExtra,  Code) :-
    option(debug(true), Request),
    setup_call_cleanup(
        ( new_memory_file(MemFile),
          open_memory_file(MemFile, write, Handle)
        ),
        http_post_data(Data, Handle, [status(Code) | HdrExtra] ),
        close(Handle)
    ),
    setup_call_cleanup(
        open_memory_file(MemFile, read, RdHandle,
                         [ free_on_close(true)
                         ]),
        copy_stream_data(RdHandle, current_output),
        close(RdHandle)),
    !.
 http_response(_, Data, HdrExtra, Code) :-
    (   number(Code)
    ->  N = Code
    ;   http_header:status_number(Code,N) 
    ),
    N < 300,
    http_post_data(Data, current_output, [status(Code) | HdrExtra] ),
    !.
 http_response(Request, json(Data), HdrExtra, Code) :-
    atom_json_dict(Codes, Data, [as(codes)]),  % json data with non ok status seems to be not supported by built in library (??)
    http_response(Request, codes(Codes, application/json), HdrExtra, Code).
 http_response(_, Data, HdrExtra, Code) :-
    throw(http_reply(Data, [status(Code) | HdrExtra] )).

%! request_accept_languages(+Request:list, -Languages:list(atom), +Options) is semidet
%  Unifies list of `Languages` with sorted list of the languages specified in the `Request`
%  `Accept-Language` header. The languages are sorted by quality attributes and normalized
%  into lower case atoms with dash `-` being replaced by underscore. The user prefered language
%  if specified in the query variable `lang` gets the highest preference and is then the foirst 
%   member of the `Languages` list (after normalizing the language code).
%
% `Options` may be one of 
%  * `no_header(DefaultLanguage)` - specifies the default language if there is no `Accept-Language`
%    header provided with the request. The default value is atom `'*'`. If set to atom `fail`
%    then this predicate fails silently if header is not set. 
%  * `wildcard_value(Wildcard)` - specifies value to use in case of the wildcard `*` symbol used 
%    in the header. The default value is atom `'*'`.
request_accept_languages(Request, [UserNorm | Languages], Options) :-
    select(search(Queries), Request, RequestRest),
    memberchk(lang=UserLang, Queries),
    language_normalized(UserLang, UserNorm, Options),
    request_accept_languages(RequestRest, Languages, Options),
    !.
 request_accept_languages(Request, Languages, Options) :-
    memberchk(accept_language(AcceptLanguages), Request),
    split_string(AcceptLanguages, ', ', ', ', Preferences),
    maplist(language_preference(Options), UnsortedLanguages, Preferences),
    sort(2, @>=, UnsortedLanguages, LanguageQualities),
    pairs_keys(LanguageQualities, Languages),
    !.
 request_accept_languages(Request, [Language], Options) :-
    \+   memberchk(accept_language(_), Request),
    (   option(no_header(fail), Options)
    ->  !, fail
    ;   option(no_header(Default), Options, '*'),
        language_normalized(Default, Language, Options)
    ),
    !.

%! request_match_condition(+Request:list, ?CurrentTag:atom, ?LastModification:float, -WeakMatch:boolean) is det
%  Succeeds if either the `CurrentTag` matches the tags provided in `If-None-Match` or in `If-Match` 
%  headers; or succeeds if `LastModification` time stamp fulfills condition of `If-Modified-Since` or 
%  `If-Unmodified-Since`.Either `CurentTag` or `LastModification` can be unboud, in which case it is not evaluated. 
%  The order of evaluating headers is following:  `If-None-Match`, `If-Match`, `If-Unmodified-Since`, `If-Modified-Since`.
%  The predicate succeeds with the first header for which condition is valid. The `Weak` variable is bound to `true` if
%  condition was fulfilled on weak Etag
request_match_condition(Request, CurrentTag, LastModification, Weak) :-
    nonvar(CurrentTag),
    atom(CurrentTag),
    atom_codes(CurrentTag, Codes),
    !,
    once(request_match_condition(Request, Codes, LastModification, Weak)).
request_match_condition(Request, CurrentTag, _, false) :-
    nonvar(CurrentTag),
    option(if_none_match('*'), Request),
    !,
    fail.
 request_match_condition(Request, CurrentTag, _, false) :-
    nonvar(CurrentTag),
    option(if_match('*'), Request),
    !.
 request_match_condition(Request, CurrentTag, _, Weak) :-
    nonvar(CurrentTag),
    option(if_none_match(Tags), Request),
    atom_codes(Tags, TagCodes),
    !,
    \+ once(phrase(tag_values_match(CurrentTag, Weak), TagCodes, _)).
request_match_condition(Request, CurrentTag, _, Weak) :-
    nonvar(CurrentTag),
    option(if_match(Tags), Request),
    atom_codes(Tags, TagCodes),
    !,
    once(phrase(tag_values_match(CurrentTag, Weak), TagCodes, _)).
 request_match_condition(Request, _, LastModification, _) :-
    nonvar(LastModification),
    option(if_unmodified_since(Date), Request),
    parse_time(Date, rfc_1123, TimeStamp),
    !,
    LastModification =< TimeStamp.
 request_match_condition(Request, _, LastModification, _) :-
    nonvar(LastModification),
    option(if_modified_since(Date), Request),
    parse_time(Date, rfc_1123, TimeStamp),
    !,
    LastModification >= TimeStamp.
 request_match_condition(_, _, _, false). % no condition on request, therefore match succeeds 


%! request_match_language(+Request:list, +Supported:list(atom), -Language:atom) is det
%  Unifies the `Language` with one of the `Supported` languages that matches the `Request`-s 
%  `Accept-Language` header, or with the first language in the `Supported` list if there is no 
%  match. Each element of the `Supported` list must be downcased atom of the language code with 
%  dash replaced by underscore, as for example  `en_us`, or `en`.
request_match_language(Request, Supported, Language) :-
    request_accept_languages(Request, Accepts, []), 
    languages_supported_match(Accepts, Supported, Language),
    !.

%! serve_assets(+Request:list) is det
%  checks for existence of the resource from Request's URI 
%  and serves it to http server. Throws =|http_reply(not_found(Path)|= 
%  if asset does not exists. The asset path is resolved 
%  by using `asset(PathInfo)` resolution of the `absolute_file_name/1` 
%  predicate. Sets last_modified header and assumes immutable 
%  public caching. Gzip caching is by default located at `html(assets_gzip_cache)`.
% 
%  This handler also supports virtual revvying and language fallback. E.g. for asset path info `/file.3456.en.jpg` it will
%  try for existence of the files `/file.3456.jpg` and `/file.jpg` before responding with `404 Not found` reply.
%  Together with the preset public caching and the one year expiration of assets, it allows for efficient control 
%  of the internet caches.
serve_assets( Request) :-
    option(path_info(Asset), Request),
    absolute_file_name(asset(Asset), Absolute, [access(read)]),
    time_file(Absolute, FileTime),
    (   \+ request_match_condition(Request, _, FileTime, _) 
    ->  http_timestamp(FileTime, LastModified),
        throw(http_reply(not_modified, [cache_control( 'public, max-age=31536000, immutable'), last_modified(LastModified)]))
    ;   http_reply_file(asset(Asset), [headers([cache_control('public, max-age=31536000, immutable')]), cached_gzip(true)], Request)
    ). 
 serve_assets( Request) :-  % revving and language fallbacks
    select_option(path_info(Asset0), Request, Request1),
    file_name_extension( Asset1, Ext, Asset0),
    file_name_extension( Asset2, Rev, Asset1),
    Rev \= '',
    file_name_extension(Asset2, Ext, Asset),
    serve_assets([path_info(Asset)| Request1]).
 serve_assets(Request) :-
    option(path(Path), Request),
    throw(http_reply(not_found(Path))).

%! serve_health_check(+Request:list) is det
%  checks if the status of all operational parameters are healthy and temporaly valid. 
%  The operational status may be set by calling set_health_status.
serve_health_check(_) :-
    (   health_status(Value)
    ->  true
    ;   Value = _{}
    ),
    (   health_check(Value)
    ->  Status = 200
    ;   Status = 501
    ),
    reply_json(Value, [status(Status)]),
    !.   


%! set_health_status(+Status:oneof(healthy, unhealthy), +OperationalParameter:atom, +Message:string, +ValidSeconds:number ) is det
%  sets health status of the particular operational parameter. The parameter is considered healthy only for the upcomming 
%  amount of `ValidSeconds` time. If the status is not set again, then the parameter is considered unhealthy afterwards.
%  Setting `ValidSeconds` to `-1` indicates the parameter is healthy undefinitely. 
set_health_status(Status, OperationalParameter, Message, ValidSeconds ) :-
    get_time(Now),
    format_time(atom(Date), '%FT%T%z', Now, posix ),
    Block = _{ status: Status, message: Message, statusTimeStamp: Date }, 
    (   ValidSeconds >= 0
    ->  get_time(Now),
        ValidUntil is Now + ValidSeconds,
        Block1 = Block.put(validUntil, ValidUntil)
    ;   Block1 = Block
    ),    
    ParameterBlock = _{}.put(OperationalParameter,  Block1), 
    transaction(
        (   (   retract( health_status(Value) )
            ->  true
            ;   Value = _{}
            ),
            asserta( health_status(Value.put(ParameterBlock)))
        )
    ). 

%! set_healthy_status( +OperationalParameter:atom, +Message:string, +ValidSeconds:number ) is det
%  Same as `set_health_status(healthy, OperationalParameter, Message, ValidSeconds )`
set_healthy_status(OperationalParameter, Message, ValidSeconds) :-
    set_health_status(healthy, OperationalParameter, Message, ValidSeconds ).

%! set_healthy_status(+OperationalParameter:atom, +Message:string) is det
%  Same as `set_health_status(healthy, OperationalParameter, Message, -1 )`
set_healthy_status(OperationalParameter, Message) :-
    set_health_status(healthy, OperationalParameter, Message, -1 ).

%! set_healthy_status(+OperationalParameter:atom) is det
%  Same as `set_health_status(healthy, OperationalParameter, healthy, -1 )`
set_healthy_status(OperationalParameter) :-
    set_health_status(healthy, OperationalParameter, healthy, -1 ).

%! set_unhealthy_status(+OperationalParameter:atom, +Message:string) is det
%  Same as `set_health_status(unhealthy, OperationalParameter, Message, -1 )`
set_unhealthy_status(OperationalParameter, Message) :-
    set_health_status(unhealthy, OperationalParameter, Message, -1 ).

%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%
health_check(Dict) :-
    dict_pairs(Dict, _, Pairs),
    maplist(health_check_component, Pairs).

health_check_component(_ - Dict) :-
    Dict.get(status) = healthy,
    (   Dict.get(validUntil) = Validity
    ->  get_time(Now),
        Now < Validity
    ;   true).


lang_norm(Lang) -->
    alpha_to_lower(C),
    lang_norm_rest(Codes),
    {   atom_codes(Lang, [C|Codes]) },
    !.
    
lang_norm_rest([C|Codes]) -->
    alpha_to_lower(C),
    lang_norm_rest(Codes).
 lang_norm_rest([0'_|Codes]) -->
    (   [0'-]
    ;   [0'_]),
    lang_norm_rest(Codes).
 lang_norm_rest([]) --> [].

language_normalized('*', Wildcard, Options) :-
    option(wildcard_value(Wildcard), Options, '*'),
    !.
 language_normalized(Text, Language, _) :-
    string_codes(Text, Codes),
    once(phrase(lang_norm(Language), Codes, _)).

language_preference(Options, Lang-Quality, Text) :-
    atom_string(Atom, Text),
    atomic_list_concat([LangText, QualityAtom], ';', Atom),
    atom_concat('q=', ValueText, QualityAtom),
    atom_number(ValueText, Quality),
    language_normalized(LangText, Lang, Options).
language_preference( Options, Lang-0.0, "*") :-
    language_normalized('*', Lang, Options).
 language_preference( Options, Lang-1.0, Text) :-
    atom_string(Atom, Text),
    language_normalized(Atom, Lang, Options).

languages_supported_match([], [Lang|_], Lang).
 languages_supported_match([Lang|_], SystemLanguages, Lang) :-
    memberchk(Lang, SystemLanguages),
    !.
 languages_supported_match([_|Preferrence], SystemLanguages, Lang) :-
    languages_supported_match(Preferrence, SystemLanguages, Lang).  

tag_values_match(Tag, false) -->
    blanks, [0'"], Tag, [0'"].
tag_values_match(Tag, false) -->
    blanks,  Tag.
tag_values_match(Tag, true) -->
    blanks, "W/",[0'"], Tag, [0'"].
tag_values_match(Tag, true) -->
    blanks, "W/", Tag.
tag_values_match(Tag, Weak) -->
    blanks, 
    ("W/"; []),
    [0'"], string_without("\"", _), [0'"],
    blanks, (","; ";"; []),
    tag_values_match(Tag, Weak).


 

    
