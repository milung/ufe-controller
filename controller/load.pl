%%% Loads all source modules to be used on top level


%  Project load search paths
:-  asserta(user:file_search_path(cliroot, project('.'))).

:- initialization(
    (     
        retractall(file_search_path(cliroot, _)),  
        retractall(file_search_path(project, _)),
        retractall(file_search_path(foreign, _)),
        current_prolog_flag(executable, ExePath),
        atomic_list_concat(Segments, '\\', ExePath),
        atomic_list_concat(Segments, '/', PosixPath),        
        directory_file_path(Dir, _, PosixPath),
        (
            directory_file_path(CliRoot, bin, Dir)
        ->  true
        ;   CliRoot = Dir
        ),        
        assert(user:file_search_path(cliroot, CliRoot)),
        assert(user:file_search_path(project, cliroot('.')))
    ),
    restore_state).

user:file_search_path(asset, html(assets)).
user:file_search_path(asset, html(modules)).
user:file_search_path(html, www).
user:file_search_path(user_home, HomeDir) :- getenv('USERPROFILE', HomeDir).
user:file_search_path(user_home, HomeDir) :- getenv('HOME', HomeDir).
user:file_search_path(config, '.').
user:file_search_path(config, './config').
user:file_search_path(config, user_home('.document-structure')).
user:file_search_path(config, cliroot(config)).
user:file_search_path(foreign, cliroot(bin)).

:- set_prolog_flag(encoding, utf8).

% standard modules for  project
:- use_module(library(settings)).
:- use_module(library(apply)).
:- use_module(library(prolog_pack)).
:- use_module(library(readutil)).
:- use_module(library(filesex)).
:- use_module(library(debug)).

%  Package management. Loads all packages specified in the 'packages.pl'
install_package(package(Name, _)) :-
    pack_property(Name, directory(_) ),
    !.
 install_package(package(Name, Url)) :-
    absolute_file_name(project('.packages'), PackageDir),
    pack_install(Name, [url(Url), package_directory(PackageDir), interactive(false)]).

:-  absolute_file_name(project('.packages'), PackageDir),
    attach_packs(PackageDir),
    make_directory_path(PackageDir),
    read_file_to_terms(project('packages.pl'), Dependencies, []),
    maplist(install_package, Dependencies),
    attach_packs(PackageDir).

% debugging is enabled for info, warnig, and error levels 
% - use debug and trace for more glanular levels
:- debug(info).
:- debug(info(_)).
:- debug(warning).
:- debug(error).
:- debug(warning(_)).
:- debug(error(_)).

% bootstrap the execution
:- use_module(source(server)).
:- use_module(source(routing)).
:- use_module(source(status_pages)).
:- use_module(source(http_extra/http_extra)).
:- use_module(source(fe_config/fe_config)).
:- use_module(source(http_extra/http_extra)).
