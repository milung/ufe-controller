:- begin_tests(fe_config).
:- use_module(source(fe_config/fe_config)).

% test(predicate, []) :-
%     fe_config:predicate(a, a).

% test('context variable retrievable ', 
%     [
%         nondet,
%         true(Value == my_value)
%     ]) 
% :-
%     % prepare
%     fe_config:prepare(Something),    
%     % execute
%     fe_config:predicate(Something, Value).

% test('multiple variables retrievable', 
%     [
%         all(Value == [my_value, my_value_2])
%     ]) 
% :-
%     % prepare
%     fe_config:prepare(Something),
%     % execute
%     fe_config:predicate(Something, Value).

% test('context_remove', 
%     [
%         fail
%     ]) :-
%     % prepare
%     fe_config:prepare(Something),
%     % execute
%     fe_config:predicate(Something).

:- end_tests(fe_config).