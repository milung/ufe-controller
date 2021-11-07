:- begin_tests(http_extra).
:- use_module(source(http_extra/http_extra)).


test('request_accept_languages', []) :-
    % prepare
    % execute
    http_extra:request_accept_languages([accept_language("sk_SK, EN_us,cz;q=0.7, sk;q=0.8, * ")], Languages, []),
    % verify
    Languages == [sk_sk, en_us, sk, cz, '*' ].

test('request_accept_languages - user query', []) :-
    % prepare
    % execute
    http_extra:request_accept_languages([accept_language("sk_SK, EN_us,cz;q=0.7, sk;q=0.8, * "), search([lang='FR-CA'])], Languages, []),
    % verify
    Languages == [fr_ca, sk_sk, en_us, sk, cz, '*' ].

test('request_accept_languages - wildcard value', []) :-
    % prepare
    % execute
    http_extra:request_accept_languages([accept_language("sk_SK, EN_us,cz;q=0.7, sk;q=0.8, * ")], Languages, [wildcard_value(en_us)]),
    % verify
    Languages == [sk_sk, en_us, sk, cz, en_us ].

test('request_accept_languages - no header', []) :-
    % prepare
    % execute
    http_extra:request_accept_languages([accept_language("sk_SK, EN_us,cz;q=0.7, sk;q=0.8, * ")], Languages, [wildcard_value(en_us)]),
    % verify
    Languages == [sk_sk, en_us, sk, cz, en_us ].

test('request_accept_languages - no header', []) :-
    % prepare
    % execute
    http_extra:request_accept_languages([], Languages, []),
    % verify
    Languages == ['*'].

test('request_accept_languages - no header +  option', []) :-
    % prepare
    % execute
    http_extra:request_accept_languages([], Languages, [no_header(en_us)]),
    % verify
    Languages == [en_us].

test('request_accept_languages - no header +  wildcard', []) :-
    % prepare

    % execute
    http_extra:request_accept_languages([], Languages, [wildcard_value(en_us)]),
    % verify
    Languages == [en_us].


test('request_accept_languages - no header +  fail', [fail]) :-
    % prepare
    http_extra:
    % execute
    http_extra:request_accept_languages([], Languages, [no_header(fail)]),
    % verify
    Languages == [en_us].

test('request_match_language', 
    [   forall(member(Accept-Lang, 
            [   "*" - en_us,
                "sk_SK, en, en_us" - en,
                "en, en_us, sk" - en,
                "cz, fr, sk, en" - sk,
                "cz, fr, *" - en_us
            ]))
    ]
) :-
    % prepare
    % execute
    http_extra:request_match_language([accept_language(Accept)], [en_us, en, sk], Lang).
    % verify
    
test('request_match_language - no header', []) :-
        % prepare
        % execute
        http_extra:request_match_language([], [en_us, en, sk], Lang),
        % verify
        Lang == en_us.

:- end_tests(http_extra).