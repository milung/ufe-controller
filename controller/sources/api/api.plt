:- begin_tests(api).
:- use_module(source(api/api)).

% test(predicate, []) :-
%     api:predicate(a, a).

% test('context variable retrievable ', 
%     [
%         nondet,
%         true(Value == my_value)
%     ]) 
% :-
%     % prepare
%     api:prepare(Something),    
%     % execute
%     api:predicate(Something, Value).

% test('multiple variables retrievable', 
%     [
%         all(Value == [my_value, my_value_2])
%     ]) 
% :-
%     % prepare
%     api:prepare(Something),
%     % execute
%     api:predicate(Something, Value).

% test('context_remove', 
%     [
%         fail
%     ]) :-
%     % prepare
%     api:prepare(Something),
%     % execute
%     api:predicate(Something).

:- end_tests(api).