% Invokes main:main/1 or if `--server` option is provided then it starts http server. 
%
%  DO NOT PLACE OTHER CODE HERE
%  instead update server.pl if necessary
%
:- [load].
:- use_module(library(execution_cli)).
:- use_module(library(apply)).  

:- initialization( execute, main).

%%% PUBLIC PREDICATES %%%%%%%%%%%%%%%%%%%%%%%%%%%

execute :- 
   current_prolog_flag(environment, production)
   -> (  execute_cli
      -> print_message(informational, format('Command succeeded', [])),
         halt(0)
      ;  print_message(error, format('Command failed', [])),
         halt(1)
      ) 
   ;  true.
   
