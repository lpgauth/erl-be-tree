-module(erl_betree).

-export([
    betree_make/0,
    betree_free/1,
    betree_add_domain/2,
    betree_add_domains/2,
    betree_insert/3,
    betree_search/2,
    betree_search_with_term/2,
    betree_delete/2
]).

betree_make() ->
    erl_betree_nif:betree_make().
betree_free(Betree) ->
    erl_betree_nif:betree_free(Betree).
betree_add_domain(Betree, Domain) ->
    erl_betree_nif:betree_add_domain(Betree, Domain).
betree_add_domains(Betree, Domains)->
    erl_betree_nif:betree_add_domains(Betree, Domains).
betree_insert(Betree, SubId, Expr) ->
    erl_betree_nif:betree_insert(Betree, SubId, Expr).
betree_search(Betree, Event) ->
    erl_betree_nif:betree_search(Betree, Event).
betree_search_with_term(Betree, Term) ->
    erl_betree_nif:betree_search_with_term(Betree, Term).
betree_delete(Betree, SubId) ->
    erl_betree_nif:betree_delete(Betree, SubId).

