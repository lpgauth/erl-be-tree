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

atom_domain_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    Domains = 
        [
            {integer, int, allow_undefined},
            {integer2, int, disallow_undefined, 0, 10},
            {integer_list, int_list, allow_undefined},
            {integer_list2, int_list, disallow_undefined, 0, 10},
            {boolean, bool, allow_undefined},
            {boolean2, bool, disallow_undefined},
            {string, bin, allow_undefined},
            {string2, bin, disallow_undefined, 5},
            {string_list, bin_list, allow_undefined},
            {string_list2, bin_list, disallow_undefined, 5},
            {float, float, allow_undefined},
            {float2, float, disallow_undefined, 0.0, 10.0},
            {frequency, frequency_caps, allow_undefined},
            {frequency2, frequency_caps, disallow_undefined},
            {segment, segments, allow_undefined},
            {segment2, segments, disallow_undefined}
        ],
    ok = erl_betree:betree_add_domains(Betree, Domains).

