:- module(fe_config, [
    serve_fe_config/1
    ]).

%! <module> Predicates for observing the K8s resources and transforming it into the 
%  configuration for the microfrontends

:- use_module(library(http/http_json)).
:- use_module(library(k8s_client)).


%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%

% %! predicate is det
% %  Lorem ipsum dolores
% predicate :- !.

serve_fe_config(_) :-
    get_config(Config, []), 
    reply_json(Config).
%%% PRIVATE PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%


get_config(Config, Options) :-
  findall(
    Resource, 
    k8s_get_resource('fe.milung.eu', v1, webcomponents, _, Resource, Options),
    Resources ),
    foldl(resource_config, Resources, _{ apps: [], preload:[] }, Config).

resource_config(Resource, In, Out) :-
    resource_config_app(Resource, In, Cfg),
    resource_config_preload(Resource, Cfg, Out).

resource_config_app(Resource, CfgIn, CfgOut) :-
    Navigations = Resource.get(spec).get(navigation),
    foldl(resource_navigation_config(Resource), Navigations, CfgIn, CfgOut ).

resource_navigation_config(Resource, Navigation, CfgIn, CfgOut ) :-
    atomic_list_concat(['/web-components/',  Resource.metadata.name, '.jsm'], ModuleUri),
    (   Roles = Navigation.get(roles)
    ->  split_string(Roles, ",; ", ",; ", RolesList)
    ;   RolesList = ['*']
    ),
    (   Labels = Resource.metadata.get(labels)
    ->  true
    ;   Labels = []),
    
    App = _{
        title: Navigation.title,
        path: Navigation.path,
        element: Navigation.element,
        load_url: ModuleUri, 
        roles: RolesList, 
        labels: Labels
    },
    CfgOut = CfgIn.put(apps, [App | CfgIn.apps ]),
    !. 

resource_config_preload(Resource, CfgIn, CfgOut) :-
    (   true = Resource.spec.get(preload)
    ->  atomic_list_concat(['/web-components/',  Resource.metadata.name, '.jsm'], ModuleUri),
        CfgOut = CfgIn.put(preload, [ModuleUri | CfgIn.preload ])
    ;   CfgOut = CfgIn
    ).

