:- module(fe_config, [
    serve_fe_config/1,
    serve_webcomponents/1,
    start_fe_config_controller/0,
    stop_fe_config_controller/0
    ]).

%! <module> Predicates for observing the K8s resources and transforming it into the 
%  configuration for the microfrontends

:- use_module(library(http/http_json)).
:- use_module(library(k8s_client)).
:- use_module(library(memfile)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_header)).
:- use_module(library(uri)).
:- use_module(library(http/http_dispatch)).

:- use_module(source(http_extra/http_extra)).

:- dynamic 
    k8s/3,
    config_cache/1,
    watcher_exit/1.

:- initialization(start_fe_config_controller).
:- at_halt(stop_fe_config_controller).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

%! serve_fe_config(+Request) is det
%  Serves the Frontend configuration file as collected by the k8s controller
serve_fe_config(_) :-
    config_cache(Config), 
    reply_json(Config).

serve_webcomponents(Request) :-
    webcomponent_uri(Request, Uri),
    http_get(Uri, Bytes, [
            to(codes),
            input_encoding(octet),
            status_code(Status),
            header(content_type, ContentType),
            timeout(10)]),
    (   Status = 404
    ->  http_404([], Request)
    ;   between(500, 599, Status)
    ->  format( atom(Msg), 'The web-component gateway returned ~w', [Status]),
        http_response(Request, Msg, [],  502)
    ;   http_response(Request, bytes(ContentType, Bytes), [], Status)
    
    ).

%! start_fe_config_controller is det
% Starts a thread that observes the web component for FE registrations at k8s API
start_fe_config_controller :-
    (   watcher_exit(_)
    ->  print_message(warning, controller(already_started)), 
        fail
    ;   true
    ),
    k8s_watch_resources_async(k8s_observer, 'fe.milung.eu', v1, webcomponents, Exit, [timeout(15)]),
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
    foldl(resource_config, Resources, _{ apps: [], preload:[] }, Config),
    transaction(
        (   retractall(config_cache(_)),
            assert(config_cache(Config))
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

prolog:message(controller(already_started)) -->
    [ 'uFE Controller thread already running, stop it first'].

resource_config(Resource, In, Out) :-
        resource_config_app(Resource, In, Cfg),
        resource_config_preload(Resource, Cfg, Out).

resource_config_app(Resource, CfgIn, CfgOut) :-
    Navigations = Resource.get(spec).get(navigation),
    foldl(resource_navigation_config(Resource), Navigations, CfgIn, CfgOut ).

resource_navigation_config(Resource, Navigation, CfgIn, CfgOut ) :-
    atomic_list_concat(['/web-components/',  Resource.metadata.name, '/',  Resource.metadata.name, '.jsm'], ModuleUri),
    (   Roles = Navigation.get(roles)
    ->  split_string(Roles, ",; ", ",; ", RolesList)
    ;   RolesList = ['*']
    ),
    (   Labels = Resource.metadata.get(labels)
    ->  true
    ;   Labels = []),
    (   Attributes = Navigation.get(attributes)
    ->  true
    ;   Attributes = []),
    
    App = _{
        title: Navigation.title,
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
    ->  atomic_list_concat(['/web-components/',  Resource.metadata.name, '.jsm'], ModuleUri),
        CfgOut = CfgIn.put(preload, [ModuleUri | CfgIn.preload ])
    ;   CfgOut = CfgIn
    ).

webcomponent_uri(Request, Uri) :-
    option(path_info(Path), Request),
    atomic_list_concat(['',Component,SubPath], '/', Path),
    k8s(_, Component, Resource),
    atomic_list_concat([Component, '.jsm'], SubPath),
    Uri = Resource.spec.'module-uri',
    !.
 webcomponent_uri(Request, Uri) :-
  option(path_info(Path), Request),
    atomic_list_concat(['',Component|SubPath], '/', Path),
    k8s(_, Component, Resource),
    ModuleUri = Resource.spec.'module-uri',    
    atomic_list_concat(SubPath, '/', Relative),
    uri_resolve(Relative, ModuleUri, Uri),
    !.

