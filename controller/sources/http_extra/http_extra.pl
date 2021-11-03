:- module(http_extra, [
    http_response/2,            % +Request:list, +Data
    http_response/3,            % +Request:list, +Data, +HdrExtra:list
    http_response/4,            % +Request:list, +Data, +HdrExtra:list, +Status
    request_match_condition/4,  % +Request:list, ?CurrentTag:atom, ?LastModification:float, -WeakMatch:boolean
    serve_assets/1              % +Request:list
    ]).
%! <module> http_extra predicates

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/json)).
:- use_module(library(dcg/basics)).

user:file_search_path(http_gzip_cache, asset(cache)).

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
    http_post_data(Data, current_output, [status(Code) | HdrExtra] ).

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
 
%! serve_assets(+Request:list) is det
%  checks for existence of the resource from Request's URI 
%  and serves it to http server. Throws =|http_reply(not_found(Path)|= 
%  if asset does not exists. The asset path is resolved by 
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
    
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%

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