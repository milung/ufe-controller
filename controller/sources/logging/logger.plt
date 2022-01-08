:- begin_tests(logger).
:- use_module(source(logger)).

% test(predicate, []) :-
%     logger:predicate(a, a).

% test('context variable retrievable ', 
%     [
%         nondet,
%         true(Value == my_value)
%     ]) 
% :-
%     % prepare
%     logger:prepare(Something),    
%     % execute
%     logger:predicate(Something, Value).

% test('multiple variables retrievable', 
%     [
%         all(Value == [my_value, my_value_2])
%     ]) 
% :-
%     % prepare
%     logger:prepare(Something),
%     % execute
%     logger:predicate(Something, Value).

% test('context_remove', 
%     [
%         fail
%     ]) :-
%     % prepare
%     logger:prepare(Something),
%     % execute
%     logger:predicate(Something).

:- end_tests(logger).