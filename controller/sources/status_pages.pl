:- module(status_pages, []).

:- multifile http:status_page/3.

http:status_page(not_found(URL), _, HTML) :-
 HTML = page(
    [   title('404 Not Found') ],
    [   h1('Not Found'),
        p(['The  page ', tt(URL),
        ' was not found on this server'
        ]),
        \address
    ]).