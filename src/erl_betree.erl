-module(erl_betree).

-export([
    betree_make/0,
    betree_free/1,
    betree_add_domains/2,
    betree_insert/4,
    betree_search/2,
    betree_delete/2,
    betree_change_boundaries/2
]).

betree_make() ->
    erl_betree_nif:betree_make().
betree_free(Betree) ->
    erl_betree_nif:betree_free(Betree).
betree_add_domains(Betree, Domains)->
    erl_betree_nif:betree_add_domains(Betree, Domains).
betree_insert(Betree, SubId, Constants, Expr) ->
    erl_betree_nif:betree_insert(Betree, SubId, Constants, Expr).
betree_search(Betree, Event) ->
    erl_betree_nif:betree_search(Betree, Event).
betree_delete(Betree, SubId) ->
    erl_betree_nif:betree_delete(Betree, SubId).
betree_change_boundaries(Betree, Expr) ->
    erl_betree_nif:betree_change_boundaries(Betree, Expr).

