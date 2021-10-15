:- use_module(library(openapi)).
:- use_module(library(option)).
:- use_module(library(debug)).

:- openapi_server(assets('openapi.yaml'), []).

%! search_inventory(+Skip, +Limit, -Response, +Options) is det.
%
%  searches inventory
%  By passing in the appropriate options, you can search for
%  available inventory in the system
%
%  @arg Skip numeric(int32,min(0))
%       number of records to skip for pagination
%  @arg Limit numeric(int32,between(0,50))
%       maximum number of records to return
%  @arg Response array(object([p(id,string,true),p(manufacturer,object([p(homePage,uri,false),p(name,string,true),p(phone,string,false)]),true),p(name,string,true),p(releaseDate,date_time,true)]))
%       search results matching criteria
%  @arg Options 
%       - searchString(+string)
%         pass an optional search string for looking up inventory
%
%  @see Path = /inventory

search_inventory(Skip, Limit, Response, Options) :-
    debug(openapi, "~p", [search_inventory(Skip, Limit, Response, Options)]),
    Response = status(404).

%! add_inventory(+RequestBody, -Response) is det.
%
%  adds an inventory item
%  Adds an item to the system
%
%  @arg RequestBody object([p(id,string,true),p(manufacturer,object([p(homePage,uri,false),p(name,string,true),p(phone,string,false)]),true),p(name,string,true),p(releaseDate,date_time,true)])
%       Inventory item to add
%  @arg Response -
%       item created
%
%  @see Path = /inventory

add_inventory(RequestBody, Response) :-
    debug(openapi, "~p", [add_inventory(RequestBody, Response)]),
    Response = status(404).

