%% used to initialize the prolog environment

:-  prolog_load_context(directory, Dir), 
    asserta(user:file_search_path(project, Dir)),
    asserta(user:file_search_path(source, project(sources))).    

:- set_prolog_flag(encoding, utf8),
   set_prolog_flag(stack_limit, 4294967296).

:- attach_packs('.packages').

:-      current_prolog_flag(gui, true)
    ->  guitracer
    ;   true.