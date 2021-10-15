   % Builds standalone executable in the .bin folder. 
%
%  DO NOT PLACE OTHER CODE HERE
%  instead update server.pl if necessary
%
:- [run].

:- use_module(library(apply)).

:- multifile user:file_search_path/2.

:- initialization((current_prolog_flag(verbose, normal);true)).

:- register_cli_command('prolog', prolog_command, []).


copy_assets :-
   absolute_file_name(asset('.'), Assets ),
   copy_directory(Assets, '.bin/assets'),
   absolute_file_name(asset('README.md'), Readme ),
   copy_file(Readme, '.bin/'),
   absolute_file_name(asset('document-structure.cmd'), Script ),
   copy_file(Script, '.bin/').

copy_config :-
   make_directory_path('.bin/config'),
   copy_file('config/config.env', '.bin/config/config.env').
copy_foreign :-
   findall(L, current_foreign_library(L,_), Foreign),
   maplist(copy_lib, Foreign).

copy_lib(foreign(Library)) :-  
   (   current_prolog_flag(windows, true)
   ->  file_name_extension(Library, 'dll', FileName)
   ;   FileName = Library
   ),   
   absolute_file_name(foreign(FileName), Absolute ),
   copy_file(Absolute, '.bin/bin').

link_executable :-   
   make_directory_path('.bin/bin'),
   qsave_program('.bin/bin/document-structure',
       [
           stack_limit(4294967296),
           stand_alone(true),  
           class(runtime)
       ]),
   copy_foreign,
   copy_assets,
   copy_config,
   !. 

prolog_command(_,_) :- prolog.

:- link_executable.