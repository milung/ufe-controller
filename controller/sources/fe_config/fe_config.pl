:- module(fe_config, [
    serve_app_icons/1,
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
:- use_module(source(logging/logger)).
:- dynamic 
    k8s/3,
    config_cache/3, % Config:dict, Etag:atom, LastModifiet:float
    watcher_exit/1.


:- initialization(start_fe_config_controller, program).
:- at_halt(stop_fe_config_controller).

%%%%%%% CONTEXT VARIABLES %%%%%%%%%%%%%%%%%%%%%%%

 :- context_variable(namespaces, list(atom), [
     env('OBSERVE_NAMESPACES'), default('*'),
     describe('Comma separated list of namespaces in which to look for webcomponents to be served by this instance')
     ]).
 :- context_variable(user_id_header, atom, [
     env('USER_ID_HEADER'), default('x-forwarded-user'),
     describe('incomming request`s header name (lowercase) specifying the user identifier, typically email')
     ]).
:- context_variable(user_email_header, atom, [
    env('USER_EMAIL_HEADER'), default('x-forwarded-email'),
    describe('incomming request`s header name (lowercase) specifying the user identifier, typically email')
    ]).
 :- context_variable(user_name_header, atom, [
     env('USER_NAME_HEADER'), default('x-forwarded-preferred-username'),
     describe('incomming request`s header name (lowercase) specifying the user name')
     ]).
 :- context_variable(user_roles_header, atom, [
     env('USER_ROLES_HEADER'), default('x-forwarded-groups'),
     describe('incomming request`s header name (lowercase) specifying the list of user roles (or groups)')
     ]).
:- context_variable(forced_refresh_period, number, [
     env('FORCED_REFRESH_PERIOD_SECONDS'), default(60),
     describe('Period in seconds, when the configuration is forced to be refreshed independently of the k8s watching status')
     ]).     

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

 serve_app_icons(Request) :-
    option(path_info(Path), Request),
    atomic_list_concat(['', NavigationPath| _], '/', Path),
    app_icon(NavigationPath, Mime, Payload, ETag),
    (   request_match_condition(Request, ETag, _, _)
    ->  http_response(
            Request, 
            bytes(Mime, Payload), 
            [
                cache_control('public, max-age=604800'), 
                etag(ETag)
            ], 
            ok
        )
    ;   throw( http_reply(
            not_modified,
            [   cache_control('public, max-age=604800'), 
                etag(ETag)
            ]
        ))
    ),
    !.
  serve_app_icons(_) :-
    throw( http_reply(not_found)).

 %! serve_fe_config(+Request) is det
 %  Serves the Frontend configuration file as collected by the k8s controller
 serve_fe_config(Request) :-
     config_cache(_, Etag, LastModifiedStamp), 
     \+ request_match_condition(Request, Etag, LastModifiedStamp, _),
     http_timestamp(LastModifiedStamp, LastModified),  
     throw(http_reply(not_modified,[cache_control('private, max-age=5'), etag(Etag), last_modified(LastModified)])).
  serve_fe_config(Request) :-
     config_cache(Config, Etag, LastModifiedStamp), 
     user_request_config(Request, Config, UserConfig),
     http_timestamp(LastModifiedStamp, LastModified),
     http_response(
         Request, 
         json(UserConfig), 
         [
             cache_control('private, max-age=5'), 
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
    log(info, fe_config, "Retrieving web component data at ~w", [Uri], []),
    (   Hash \= []
    ->  Headers = [cache_control('public, max-age=31536000, immutable'), etag(Hash)]
    ;   Headers = []
    ),
    http_response( Request, forward(Uri, RequestHeaders), Headers, ok ).
   

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
    % the configuration is healthy after retrieved from watcher callback
    set_unhealthy_status(web_component_watcher, "The watcher not started"),
    set_healthy_status(fe_config, "No change was detected"),
    % Observe Kubernetes API
    k8s_watch_resources_async(
        k8s_observer, 
        'fe.milung.eu', 
        v1, 
        webcomponents, 
        Exit, 
        [
            timeout(15), 
            heartbeat_callback(http_extra:set_healthy_status(web_component_watcher, "Watching WebComponents", 30))
        ]),
    thread_create(
        k8s_refresh_loop, _, 
        [ 
            alias(k8s_refresh_loop), 
            detached(true), 
            at_exit(print_message(warning, ufe_controller(refresh_loop_exited)))
        ]),
    print_message(information, ufe_controller(started)), 
    assertz(watcher_exit(Exit)).

 %! stop_fe_config_controller is det
 %  Stops a thread that observes the web component for FE registrations at k8s API
 stop_fe_config_controller :-
     retract(watcher_exit(ExitGoal)),
     call(ExitGoal).

%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%
app_icon(AppPath, ContentType, Bytes, ETag) :-
    k8s(_, _, Resource),
    member(Navigation, Resource.get(spec/navigation)),
    atom_string(AppPath, Navigation.get(path)),
    ContentType = Navigation.get(icon/mime),
    app_icon_data(Navigation, Bytes, ETag),
    !.

app_icon_data(Navigation, Bytes, ETag) :-
    Data0 = Navigation.get(icon/data),
    string_codes(Data0, Codes0),
    remove_ws(Codes0, Data1),
    md5_hash(Data1,ETag, [enoding(octet)]),
    string_codes(Data, Data1),
    base64(BytesStr, Data),
    string_codes(BytesStr, Bytes),
    !.
app_icon_data(Navigation, Bytes, ETag) :-
    URL = Navigation.get(icon/url),
    http_get(URL, Bytes, [ to(codes), header(etag, ETag)]),
    !.

remove_ws([], []).
 remove_ws([C|In], Out) :-
    (  is_white(C)
    -> Out=Tail
    ;  Out = [C|Tail]
    ),
    remove_ws(In, Tail).


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
    ),
    set_healthy_status(fe_config, 'The configuration was updated successfully'),
    !.
 fe_config_update :-
    set_unhealthy_status(fe_config, 'The configuration update failed'),
    !,
    fail.

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

k8s_refresh_loop :-
    context_variable_value(forced_refresh_period, Period),    
    repeat,
    sleep(Period),
    catch(
        (
            k8s_refresh_webc_list,
            print_message(information, ufe_controller(refresh_loop))
        ), 
        Error, 
        (
            print_message(error, Error),
            set_unhealthy_status(web_component_watcher, "The watcher failed")
        )
    ),
    fail.

k8s_refresh_webc_list :-
    findall(
        Resource,
        k8s_get_resource(
            'fe.milung.eu', 
            v1, 
            webcomponents,
            _InstanceName, 
            Resource, 
            []
        ),
        Resources
    ),
    refresh_storage(Resources, WasChanged),
    (   WasChanged
    ->  fe_config_update
    ;   true
    ).  

refresh_storage([], false).
 refresh_storage([Resource|Resources], WasChanged) :-
    atom_string(Namespace, Resource.metadata.namespace),
    atom_string(Name, Resource.metadata.name), 
    k8s(Namespace, Name, Old),
    OldVersion = Old.get(metadata/resourceVersion),
    NewVersion = Resource.get(metadata/resourceVersion),
    NewVersion = OldVersion,
    !,
    refresh_storage(Resources, WasChanged).
  refresh_storage([Resource|Resources], true) :-
    atom_string(Namespace, Resource.metadata.namespace),
    atom_string(Name, Resource.metadata.name),
    retractall(k8s(Namespace, Name, _)),
    assertz(k8s(Namespace, Name, Resource)),
    refresh_storage(Resources, _).

:- multifile
    prolog:message//1,
    prolog:error_message//1,
    prolog:message_context//1.

prolog:message(ufe_controller(started)) -->
    [ 'uFE Controller thread started watching webcomponent resources'].
prolog:message(ufe_controller(refresh_loop)) -->
    [ 'uFE Controller - list of web components refreshed'].
prolog:message(ufe_controller(refresh_loop_exited)) -->
    [ 'uFE Controller refresh thread exited'].  
prolog:message(ufe_controller(already_started)) -->
    [ 'uFE Controller thread already running, stop it first'].

pass_through(accept_language).
pass_through(accept_charset).

pass_through_match(HeaderValue, request_header(Header=Value) ) :-
    HeaderValue =.. [ Header, Value],
    pass_through(Header),
    !.

:- table(renase_uri/2). 

rebase_uri(Uri, Rebased) :-
    context_variable_value(server:server_base_url, Base),
    (   atom_concat(BaseShort, '/', Base) 
    ->  BaseFull = Base
    ;   atom_concat(Base, '/', BaseFull),
        BaseShort = Base
    ),
    (   atom_concat('/', _, Uri)
    ->  atom_concat(BaseShort, Uri, Rebased)
    ;   atom_concat(BaseFull, Uri, Rebased)
    ),
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
    Contexts = Resource.get(spec).get(contextElements),
    foldl(resource_context_config(Resource), Contexts, CfgIn, CfgOut ),
    !.
resource_config_ctx(_, Cfg, Cfg).

resource_context_config(Resource, Context, CfgIn, CfgOut ) :-
    resource_moduleUri( Resource, ModuleUri),
    resource_styles( Resource, ModuleUri, Styles),
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
        styles: Styles,
        roles: RolesList, 
        labels: Labels,
        attributes: Attributes
    },
    CfgOut = CfgIn.put(contexts, [Ctx | CfgIn.contexts ]),
    !.

resource_navigation_config(Resource, Navigation, CfgIn, CfgOut ) :-
    resource_moduleUri( Resource, ModuleUri),
    resource_styles( Resource, ModuleUri, Styles),
    (   Roles = Navigation.get(roles) 
    ->  split_string(Roles, ",; ", ",; ", RolesList)
    ;   RolesList = ['*']
    ),
    (   Labels = Resource.metadata.get(labels) ->  true ;   Labels = []),
    (   Attributes = Navigation.get(attributes) ->  true;   Attributes = [] ),
    (   Details = Navigation.get(details) ->  true ;   Details = '' ),
    (   Priority = Navigation.get(priority) ->  true;   Priority = 0),
    (   atom_concat( IconPath, '/', Navigation.path)
    ->  NavPath =  Navigation.path
    ;   atom_concat(Navigation.path, '/', NavPath),
        IconPath = Navigation.path
    ),
    App0 = _{
        title: Navigation.title,
        details: Details,
        path: NavPath,
        priority: Priority,
        element: Navigation.element,
        load_url: ModuleUri, 
        styles: Styles,
        roles: RolesList, 
        labels: Labels,
        attributes: Attributes
    },
    (   _ = Navigation.get(icon)
    ->  rebase_uri('app-icons/', Base),
        atomic_list_concat( [Base, IconPath], IconPath1),         
        App = App0.put(icon, IconPath1)
    ;   App = App0
    ),
    CfgOut = CfgIn.put(apps, [App | CfgIn.apps ]),
    
    !. 

resource_config_preload(Resource, CfgIn, CfgOut) :-
    (   true = Resource.spec.get(preload)
    ->  resource_moduleUri( Resource, ModuleUri),
        resource_styles( Resource, ModuleUri, Styles),
        CfgOut = CfgIn.put(preload, [ _{ load_url: ModuleUri, styles: Styles} | CfgIn.preload ])
    ;   CfgOut = CfgIn
    ).


resource_moduleUri(Resource, ModuleUri) :-
    (  atom_string('built-in', Resource.spec.'module-uri')
    -> ModuleUri = ''
    ;  (   atom_string(true, Resource.spec.get(proxy))
       ->  (   Suffix0 = Resource.spec.get('hash-suffix')
           ->  atomic_list_concat(['.', Suffix0], Suffix)
           ;   Suffix = ''
           ),
           rebase_uri('/web-components/', WebCompUri),
           atomic_list_concat([
               WebCompUri,
               Resource.metadata.namespace, '/',
               Resource.metadata.name, '/',  
               Resource.metadata.name, Suffix, '.jsm'], ModuleUri)
       ;   atom_string( ModuleUri, Resource.spec.'module-uri')
       )
    ).

resource_styles(Resource, ModuleUri, AbsStyles) :-
    (  Styles = Resource.spec.get('style-relative-paths')
    -> true
    ;   Styles = []
    ),
    maplist({ModuleUri}/[R,A] >> uri_resolve(R, ModuleUri, A), Styles, AbsStyles ).

request_header_value(Request, HeaderName, Value) :-
    atom_codes(HeaderName, Codes),
    phrase(http_header:field_name(Name), Codes),
    Opt =..[Name, Value],
    memberchk(Opt, Request),
    !.

request_header_value(Request, HeaderName, Value, Default) :-
    request_header_value(Request, HeaderName, Value)
    -> true
    ;  Value = Default.

user_request_config(Request, Config, UserConfig) :-
    context_variable_value(user_id_header, IdHeader),
    context_variable_value(user_email_header, EmailHeader),
    context_variable_value(user_name_header, NameHeader),
    context_variable_value(user_roles_header, RolesHeader),
    request_header_value(Request, IdHeader, UserId),
    request_header_value(Request, EmailHeader, UserEmail),
    request_header_value(Request, NameHeader, UserName, UserId),
    request_header_value(Request, RolesHeader, UserRoles, ''),
    UserConfig = Config.put(_{
        user: _{
            name: UserName,
            id: UserId,
            roles: UserRoles,
            email: UserEmail
        }
    }),
    !.
 user_request_config(_, Config, UserConfig) :-
    UserConfig = Config.put(_{
        anonymous: true
    }).

    
webcomponent_uri(Request, Uri, Hash) :-
    option(path_info(Path), Request),
    atomic_list_concat(['',Namespace, Component,_], '/', Path),
    k8s(Namespace, Component, Resource),
    (   Hash = Resource.spec.get('hash-suffix') 
    ->  true 
    ;   Hash = []
    ),
    resource_moduleUri(Resource, ModuleUri),
    rebase_uri('/web-components', WebCompUri),

    atom_concat(WebCompUri, Path, ModuleUri),
    Uri = Resource.spec.'module-uri',
    !.
 webcomponent_uri(Request, Uri, []) :-
    option(path_info(Path), Request),
    atomic_list_concat(['',Namespace, Component|SubPath], '/', Path),
    k8s(Namespace, Component, Resource),
    ModuleUri = Resource.spec.'module-uri',    
    atomic_list_concat(SubPath, '/', Relative),
    uri_resolve(Relative, ModuleUri, Uri),
    !.

