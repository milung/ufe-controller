:- module(fe_config, [
    serve_fe_config/1,
    serve_webcomponents/1,
    start_fe_config_controller/0,
    stop_fe_config_controller/0
    ]).

%! <module> Predicates for observing the K8s resources and transforming it into the 
%  configuration for the microfrontends

:- use_module(library(http/http_client)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(k8s_client)).
:- use_module(library(md5)).
:- use_module(library(memfile)).
:- use_module(library(uri)).
:- use_module(library(execution_context)).

:- use_module(source(http_extra/http_extra)).

:- dynamic 
    k8s/3,
    config_cache/3, % Config:dict, Etag:atom, LastModifiet:float
    watcher_exit/1.


:- initialization(start_fe_config_controller, program).
:- at_halt(stop_fe_config_controller).


:- multifile http_header:field_name//1.

http_header:field_name(etag) --> "ETag".

:- context_variable(namespaces, list(atom), [
    env('OBSERVE_NAMESPACES'), default('*'),
    describe('Comma separated list of namespaces in which to look for webcomponents to be served by this instance')
    ]).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! serve_fe_config(+Request) is det
%  Serves the Frontend configuration file as collected by the k8s controller
serve_fe_config(Request) :-
    config_cache(_, Etag, LastModifiedStamp), 
    \+ request_match_condition(Request, Etag, LastModifiedStamp, _),
    http_timestamp(LastModifiedStamp, LastModified),  
    throw(http_reply(not_modified,[cache_control('public, max-age=3600'), etag(Etag), last_modified(LastModified)])).
 serve_fe_config(Request) :-
    config_cache(Config, Etag, LastModifiedStamp), 
    http_timestamp(LastModifiedStamp, LastModified),
    http_response(
        Request, 
        json(Config), 
        [
            cache_control('public, max-age=3600'), 
            etag(Etag), 
            last_modified(LastModified)
        ], 
        ok).

serve_webcomponents(Request) :-
    webcomponent_uri(Request, _, ETag),
    (   ETag \= []
    ->  \+ request_match_condition(Request, ETag, _, _),
        
        throw(http_reply(not_modified, [cache_control('public, max-age=31536000, immutable'), etag(ETag)]))
    ).
 serve_webcomponents(Request) :-
    webcomponent_uri(Request, Uri, Hash),
    request_pass_through_headers(Request, RequestHeaders),
    http_get(
        Uri, Bytes, 
        [
            to(codes),
            input_encoding(octet),
            status_code(Status),
            header(content_type, ContentType),
            header(etag, EtagExt),
            header(last_modified, LastModifiedExt),
            header(cache_control, CacheControlExt), 
            timeout(10) 
            | RequestHeaders
        ]
    ),
    (   Status = 404
    ->  http_404([], Request)
    ;   between(500, 599, Status)
    ->  format( atom(Msg), 'The web-component gateway returned ~w', [Status]),
        http_response(Request, Msg, [],  502)
    ;   (   Hash \= []
        ->  Headers = [cache_control('public, max-age=31536000, immutable'), etag(Hash)]
        ;   % copy caching control headers from the remote provider
            Headers0 = [],
            ( nonvar(EtagExt) -> Headers1 = [etag(EtagExt)| Headers0]; Headers1 = Headers0 ),
            ( nonvar(LastModifiedExt) -> Headers2 = [last_modified(LastModifiedExt)| Headers1] ; Headers2 = Headers1 ),
            ( nonvar(CacheControlExt) -> Headers = [cache_control(CacheControlExt)| Headers2] ; Headers = Headers2 )
        ),
        http_response(Request, bytes(ContentType, Bytes), Headers, Status)
    ).
 
%! start_fe_config_controller is det
% Starts a thread that observes the web component for FE registrations at k8s API
start_fe_config_controller :-
    (   watcher_exit(_)
    ->  print_message(warning, ufe_controller(already_started)), 
        fail
    ;   true
    ),
    % initial state
    get_time(Now),
    assertz(config_cache(
        _{ apps: [], preload:[] },
        'Tk8gUkVTT1VSQ0VTIERFVEVDVEVEIFlFVA',
        Now)
    ),
    % Observe Kubernetes API
    k8s_watch_resources_async(k8s_observer, 'fe.milung.eu', v1, webcomponents, Exit, [timeout(15)]),
    print_message(information, ufe_controller(started)), 
    assertz(watcher_exit(Exit)).

%! stop_fe_config_controller is det
%  Stops a thread that observes the web component for FE registrations at k8s API
stop_fe_config_controller :-
    retract(watcher_exit(ExitGoal)),
    call(ExitGoal).

%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%
fe_config_update :-
    findall(
        Resource, 
        k8s(_, _, Resource),
        Resources ),
    foldl(resource_config, Resources, _{ apps: [], preload:[], contexts: [] }, Config),
    atom_json_dict(Text, Config, [as(atom)]),
    md5_hash(Text, Etag, []),
    transaction(
        (   retractall(config_cache(_, _, _)),
            get_time(LastModifiedStamp),
            assert(config_cache(Config, Etag, LastModifiedStamp))
        )
    ).

k8s_observer(added, Resource) :-
    atom_string(Namespace, Resource.metadata.namespace),
    atom_string(Name, Resource.metadata.name), 
    retractall(k8s(Namespace, Name, _)),
    assertz(k8s(Namespace, Name, Resource)),
    fe_config_update.
 k8s_observer(modified, Resource) :-
    k8s_observer(added, Resource).
 k8s_observer(deleted, Resource) :-
    atom_string(Namespace, Resource.metadata.namespace),
    atom_string(Name, Resource.metadata.name),  
    retractall(k8s(Namespace, Name, _)),
    fe_config_update.

:- multifile
    prolog:message//1,
    prolog:error_message//1,
    prolog:message_context//1.

prolog:message(ufe_controller(started)) -->
    [ 'uFE Controller thread started watching webcomponent resources'].
prolog:message(ufe_controller(already_started)) -->
    [ 'uFE Controller thread already running, stop it first'].

pass_through(accept_language).
pass_through(accept_charset).

pass_through_match(HeaderValue, request_header(Header=Value) ) :-
    HeaderValue =.. [ Header, Value],
    pass_through(Header),
    !.

request_pass_through_headers(Request, Headers) :- 
    convlist(pass_through_match, Request, Headers). 

resource_config(Resource, In, Out) :-
        resource_config_app(Resource, In, Cfg0),
        resource_config_preload(Resource, Cfg0, Cfg1),
        resource_config_ctx(Resource, Cfg1, Out).

resource_config_app(Resource, CfgIn, CfgOut) :-
    Navigations = Resource.get(spec).get(navigation),
    foldl(resource_navigation_config(Resource), Navigations, CfgIn, CfgOut ),
    !.
resource_config_app(_, Cfg, Cfg).

resource_config_ctx(Resource, CfgIn, CfgOut) :-
    Contexts = Resource.get(spec).get(contexts),
    foldl(resource_context_config(Resource), Contexts, CfgIn, CfgOut ),
    !.
resource_config_ctx(_, Cfg, Cfg).

resource_context_config(Resource, Context, CfgIn, CfgOut ) :-
    resource_moduleUri( Resource, ModuleUri),
    (   RolesList = Context.get(roles)
    ->  true
    ;   RolesList = ['*']
    ),
    (   Labels = Resource.metadata.get(labels)
    ->  true
    ;   Labels = []),
    (   ContextNames = Context.get(contextNames)
    ->  true
    ;   ContextNames = []),
    (   Attributes = Context.get(attributes)
    ->  true
    ;   Attributes = [] ),
    Ctx = _{
        contextNames: ContextNames,
        element: Context.element,
        load_url: ModuleUri, 
        roles: RolesList, 
        labels: Labels,
        attributes: Attributes
    },
    CfgOut = CfgIn.put(contexts, [Ctx | CfgIn.contexts ]),
    !.

resource_navigation_config(Resource, Navigation, CfgIn, CfgOut ) :-
    resource_moduleUri( Resource, ModuleUri),
    (   Roles = Navigation.get(roles)
    ->  split_string(Roles, ",; ", ",; ", RolesList)
    ;   RolesList = ['*']
    ),
    (   Labels = Resource.metadata.get(labels)
    ->  true
    ;   Labels = []),
    (   Attributes = Navigation.get(attributes)
    ->  true
    ;   Attributes = [] ),
    (   Details = Navigation.get(details)
    ->  true
    ;   Details = ''
    ),
    App = _{
        title: Navigation.title,
        details: Details,
        path: Navigation.path,
        element: Navigation.element,
        load_url: ModuleUri, 
        roles: RolesList, 
        labels: Labels,
        attributes: Attributes
    },
    CfgOut = CfgIn.put(apps, [App | CfgIn.apps ]),
    !. 

resource_config_preload(Resource, CfgIn, CfgOut) :-
    (   true = Resource.spec.get(preload)
    ->  resource_moduleUri( Resource, ModuleUri),
        CfgOut = CfgIn.put(preload, [ModuleUri | CfgIn.preload ])
    ;   CfgOut = CfgIn
    ).

resource_moduleUri(Resource, ModuleUri) :-
(   atom_string(true, Resource.spec.get(proxy))
->  (   Suffix0 = Resource.spec.get('hash-suffix')
    ->  atomic_list_concat(['.', Suffix0], Suffix)
    ;   Suffix = ''
    ),
    atomic_list_concat([
        '/web-components/',  
        Resource.metadata.name, '/',  
        Resource.metadata.name, Suffix, '.jsm'], ModuleUri)
;   atom_string( ModuleUri, Resource.spec.'module-uri')
).

webcomponent_uri(Request, Uri, Hash) :-
    option(path_info(Path), Request),
    atomic_list_concat(['',Component,_], '/', Path),
    k8s(_, Component, Resource),
    (   Hash = Resource.spec.get('hash-suffix') 
    ->  true 
    ;   Hash = []
    ),
    resource_moduleUri(Resource, ModuleUri),
    atom_concat('/web-components', Path, ModuleUri),
    Uri = Resource.spec.'module-uri',
    !.
 webcomponent_uri(Request, Uri, []) :-
  option(path_info(Path), Request),
    atomic_list_concat(['',Component|SubPath], '/', Path),
    k8s(_, Component, Resource),
    ModuleUri = Resource.spec.'module-uri',    
    atomic_list_concat(SubPath, '/', Relative),
    uri_resolve(Relative, ModuleUri, Uri),
    !.

