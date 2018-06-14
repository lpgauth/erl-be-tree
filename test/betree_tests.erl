-module(betree_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domain(Betree, "i|integer|false|0|10"),
    ok = erl_betree:betree_insert(Betree, 1, "i = 5"),
    {ok, Subs} = erl_betree:betree_search(Betree, "{\"i\": 5}"),
    ?assertEqual(Subs, [1]),
    ok = erl_betree:betree_free(Betree),
    ?assertEqual(true, true).

