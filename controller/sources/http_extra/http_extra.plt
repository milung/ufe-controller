:- begin_tests(http_extra).
:- use_module(source(http_extra/http_extra)).

% test(predicate, []) :-
%     http_extra:predicate(a, a).

% test('context variable retrievable ', 
%     [
%         nondet,
%         true(Value == my_value)
%     ]) 
% :-
%     % prepare
%     http_extra:prepare(Something),    
%     % execute
%     http_extra:predicate(Something, Value).

% test('multiple variables retrievable', 
%     [
%         all(Value == [my_value, my_value_2])
%     ]) 
% :-
%     % prepare
%     http_extra:prepare(Something),
%     % execute
%     http_extra:predicate(Something, Value).

% test('context_remove', 
%     [
%         fail
%     ]) :-
%     % prepare
%     http_extra:prepare(Something),
%     % execute
%     http_extra:predicate(Something).

:- end_tests(http_extra).