-module(erl_betree_nif).

-compile(no_native).
-on_load(on_load/0).

-export([
    betree_make/0,
    betree_free/1,
    betree_add_domains/2,
    betree_insert/4,
    betree_search/2,
    betree_delete/2
]).

-spec on_load() -> ok.

on_load() ->
    SoName = case code:priv_dir(erl_betree) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, erl_betree]);
                _ ->
                    filename:join([priv, erl_betree])
            end;
        Dir ->
            filename:join(Dir, erl_betree)
    end,
    ok = erlang:load_nif(SoName, 0).

%% shamelessly stolen from crypto.erl
-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

betree_make() ->
    ?nif_stub.
betree_free(_Betree) ->
    ?nif_stub.
betree_add_domains(_Betree, _Domains)->
    ?nif_stub.
betree_insert(_Betree, _SubId, _Constants, _Expr) ->
    ?nif_stub.
betree_search(_Betree, _Event) ->
    ?nif_stub.
betree_delete(_Betree, _SubId) ->
    ?nif_stub.

