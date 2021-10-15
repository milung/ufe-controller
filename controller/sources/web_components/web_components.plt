:- begin_tests(web_components).
:- use_module(source(web_components)).

% test(predicate, []) :-
%     web_components:predicate(a, a).

% test('context variable retrievable ', 
%     [
%         nondet,
%         true(Value == my_value)
%     ]) 
% :-
%     % prepare
%     web_components:prepare(Something),    
%     % execute
%     web_components:predicate(Something, Value).

% test('multiple variables retrievable', 
%     [
%         all(Value == [my_value, my_value_2])
%     ]) 
% :-
%     % prepare
%     web_components:prepare(Something),
%     % execute
%     web_components:predicate(Something, Value).

% test('context_remove', 
%     [
%         fail
%     ]) :-
%     % prepare
%     web_components:prepare(Something),
%     % execute
%     web_components:predicate(Something).

:- end_tests(web_components).