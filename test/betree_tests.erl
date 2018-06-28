-module(betree_tests).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domain(Betree, <<"i|integer|false|0|10">>),
    ok = erl_betree:betree_insert(Betree, 1, <<"i = 5">>),
    {ok, Subs} = erl_betree:betree_search(Betree, <<"{\"i\": 5}">>),
    ?assertEqual(Subs, [1]),
    ok = erl_betree:betree_free(Betree).

bad_insert_no_domain_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    ?assertEqual(error, erl_betree:betree_insert(Betree, 1, <<"i = 5">>)),
    ok = erl_betree:betree_free(Betree).

bad_insert_bounded_string_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domain(Betree, <<"s|string|false|1">>),
    ok = erl_betree:betree_insert(Betree, 1, <<"s <> \"good\"">>),
    ?assertEqual(error, erl_betree:betree_insert(Betree, 2, <<"s <> \"bad\"">>)),
    {ok, Subs} = erl_betree:betree_search(Betree, <<"{\"s\": \"diff\"}">>),
    ?assertEqual(Subs, [1]),
    ok = erl_betree:betree_free(Betree).

