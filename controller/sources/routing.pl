:- module(routing, []).
%! <module> HTTP routing 
%  Predicates for basic routing of http requests

:- encoding(utf8).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_header)).
:- use_module(library(execution_context)).
:- use_module(library(mustache)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/http_json)).

:- use_module(source(fe_config/fe_config)).
:- use_module(source(http_extra/http_extra)).
:- use_module(source(logging/logger)).
 
:- multifile 
    user:file_search_path/2,
    http:status_page/3,
    http:location/3. 

:- dynamic   
    http:location/3.

:-  initialization(( 
        context_variable_value(server:server_base_url, Base),
        set_setting(http:prefix, Base)
    )).


%%%%%%%%%   CONTEXT   VARIABLES %%%%%%%%%%%%%%%%%%%%
 :- context_variable(app_title, atom, [
    env('APPLICATION_TITLE'), 
    default('Application shell'), 
    describe(
        'Language fallback application title, language specific \c
        titles are also possible, e.g. APPLICATION_TITLE_EN_US')]).
 :- context_variable(app_title_short, atom, [
     env('APPLICATION_TITLE_SHORT'), 
     default('Shell'), 
     describe(
         'Short version of the language fallback application title, language specific \c
         titles are also possible, e.g. APPLICATION_TITLE_SHORT_EN_US')]).
 :- context_variable(app_description, atom, [
     env('APPLICATION_DESCRIPTION'), 
     default(''),
     description('Some detailed description of the applivation to be part of the `index.html` meta. Language specific descriptions are also possible, e.g. APPLICATION_DESCRIPTION_EN_US')]).
 :- context_variable(accepts_languages, list, [
     env('ACCEPTS_LANGUAGES'), 
     default(['en']), 
     describe(
         'List of semicolon, or comma separated language codes that are supported. \c
          If there is match between `Accept-Language` header and this list, then language \c
          of html element is set to such language. In case there is no match then html language \c
          is set to the first language in this list.')]).
 :- context_variable(background_color, atom, [
     env('MANIFEST_BACKGROUND_COLOR'), 
     default('#16161d'), 
     describe(
         'background color to use in the `manifest.json`')]).
 :- context_variable(theme_color, atom, [
     env('MANIFEST_BACKGROUND_COLOR'),  
     default('#16161d'), 
     describe(
         'theme color to use in the `manifest.json`')]).
 :- context_variable(csp_header, atom, [
     env('HTTP_CSP_HEADER'), 
     default(
        'default-src ''self''; \c
         font-src ''self'' data: https://fonts.googleapis.com/ https://fonts.gstatic.com/; \c
         script-src ''strict-dynamic'' ''nonce-{NONCE_VALUE}''; \c
         manifest-src ''self'' https://github.com/login/oauth/; \c
         style-src ''self'' ''unsafe-inline'' https://fonts.googleapis.com/ https://fonts.gstatic.com/;'), 
     describe(
         'Content Security Policy header directives for serving \c
          the root SPA html page. The placeholder `{NONCE_VALUE}` will be \c
          automatically replaced by the random nonce text used to \c
          augment `<script>` elements in the html file.')]).
 :- context_variable(app_shell_context, atom, [
     env('APPLICATION_SHELL_CONTEXT'), 
     default('application-shell'), 
     describe(
         'context of the dynamic web component that is used to retrieve the application shell - used to build top-level element in the page body')]).
 :- context_variable(webcomponents_selector, atom, [
     env('WEBCOMPONENTS_SELECTOR'), 
     default(''), 
     describe(
         'comma separate list of key-value pairs, used to filter WebComponent resources with coresponding filters')]).

%%%% ROUTING TABLE

 :- http_handler(root('fe-config'), logged_http(serve_fe_config), [prefix]).
 :- http_handler(root('healtz'), serve_health_check, [prefix]).
 :- http_handler('/healtz', serve_health_check, []).
 :- http_handler(root('manifest.json'), logged_http(serve_manifest), []).
 :- http_handler(root('assets'), logged_http(serve_assets), [prefix]).
 :- http_handler(root(modules), logged_http(serve_assets), [prefix]).
 :- http_handler(root('web-components'), logged_http(serve_webcomponents), [prefix]). 
 :- http_handler(root('app-icons'), logged_http(serve_app_icons), [prefix]). 
 :- http_handler(root('favicon.ico'), logged_http(http_reply_file(asset('icon/favicon.ico')), [headers([cache_control('public, max-age=31536000, immutable')]), cached_gzip(true)]), []).
 :- http_handler(root(.), logged_http(serve_spa), [prefix]). 

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%  PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%  
get_nonce(Nonce) :-
    length(Codes, 128),
    maplist(random(32, 127), Codes),
    atom_codes(Text, Codes),
    base64(Text, Nonce),
    !.

html_lang_variable(Language, _, EnvironmentName, Value) :-
    atomic_list_concat([EnvironmentName, Language], '_', ENV0), 
    getenv(ENV0, Value),
    !.
 html_lang_variable(Language, _, EnvironmentName, Value) :-
    atomic_list_concat([Lang, _], '_', Language),
    atomic_list_concat([EnvironmentName, Lang], '_', ENV0), 
    getenv(ENV0, Value),
    !.
 html_lang_variable(_, CtxName, _, Value) :-
    context_variable_value(CtxName, Value),
    !.
 html_lang_variable(_, _, _, '').

html_variables(
    Request, 
    [   'base-href'=BaseUrl, 
        'background-color' = BckColor, 
        'theme-color' = ThemeColor, 
        language = Language, 
        'app-title' = Title, 
        'app-title-short' = ShortTitle, 
        description=Description,
        'ufe-shell-context'=ShellContext,
        'ufe-selector'=Selector
    ]
) :-
    context_variable_value(server:server_base_url, BaseUrl),
    context_variable_value(background_color, BckColor),
    context_variable_value(theme_color, ThemeColor), 
    context_variable_value(accepts_languages, SupportedLangs),
    context_variable_value(webcomponents_selector, Selector),
    context_variable_value(app_shell_context, ShellContext),
    request_match_language(Request, SupportedLangs, Language), 
    html_lang_variable(Language, app_title, 'APPLICATION_TITLE', Title),
    html_lang_variable(Language, app_title_short, 'APPLICATION_TITLE_SHORT', ShortTitle),
    html_lang_variable(Language, app_description, 'APPLICATION_DESCRIPTION', Description),   
    !.

file_replace_nonce(File, Nonce, Codes) :-
    atomic_list_concat(['<script nonce="', Nonce, '" '], NonceScript),
    atom_codes(NonceScript, NonceReplace),
    absolute_file_name(File, Index),
    phrase_from_file(nonce_html(NonceReplace, Codes),  Index),
    !.
nonce_html(_, []) --> eos, !.
 nonce_html(ScriptNonce, Codes) -->
    "<script>",
    {   append(ScriptNonce, [ 0'> ], SN1),        
        append(SN1, Rest, Codes) 
    },
    !,
    nonce_html(ScriptNonce, Rest).
 nonce_html(ScriptNonce, Codes) -->
    "<script ",
    { append(ScriptNonce, Rest, Codes) },
    !,
    nonce_html(ScriptNonce, Rest).
 nonce_html(ScriptNonce, [C| Codes])  -->
    [C],
    nonce_html(ScriptNonce,  Codes).

serve_manifest( Request) :-
    html_variables(Request, Variables),
    context_variable_value(accepts_languages, SupportedLangs),
    request_match_language(Request, SupportedLangs, Language),
    asset_by_language(html('manifest.json'), Language, Asset),
    mustache_from_file(Asset, Variables, Text),
    http_response(
        Request, 
        codes('application/json; charset=UTF-8', Text),
        [ cache_control('public, max-age=3153600, immutable') ]
    ).

serve_spa( Request) :-
    context_variable_value(csp_header, Csp),
    get_nonce(Nonce),
    interpolate_string(Csp, CspNonce, ['NONCE_VALUE'=Nonce], []),
    context_variable_value(accepts_languages, SupportedLangs),
    request_match_language(Request, SupportedLangs, Language),
    asset_by_language(html('index.html'), Language, Asset),
    file_replace_nonce(Asset, Nonce, TemplateCodes),
    html_variables(Request, Variables),
    phrase(mustache(Variables, TemplateCodes), Text),
    http_response(
        Request, 
        codes('text/html; charset=UTF-8', Text),
        [ content_security_policy(CspNonce), cache_control('public, max-age=60') ]
    ).

asset_by_language(Asset, Language, Path) :-
    absolute_file_name(Asset, Path0, [access(read), file_errors(fail)]), 
    file_name_extension(Base, Ext, Path0),
    file_name_extension(Base, Language, Base2),
    file_name_extension(Base2, Ext, Path2),
    (   atomic_list_concat([Lang, _], '_', Language) 
    ->  file_name_extension(Base, Lang, Base1),
        file_name_extension(Base1, Ext, Path1)
    ;   Path1 = Path0
    ),
    (   access_file( Path2, read)
    ->  Path = Path2
    ;   access_file( Path1, read)
    ->  Path = Path1
    ;   access_file( Path0, read)
    ->  Path = Path0
    ;   fail
    ),
    !.







    