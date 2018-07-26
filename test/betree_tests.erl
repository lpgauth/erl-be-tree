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

atom_all_domain_types_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    Domains = [
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
        ]
    ],
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ok = erl_betree:betree_free(Betree).

-record(single, { a, b }).
atom_single_domain_list_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    Domains = [[
                {a, bool, disallow_undefined}, 
                {b, bool, disallow_undefined} 
               ]],
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, <<"a and b">>)),
    {ok, Subs} = erl_betree:betree_search_with_term(Betree, [#single{ a = true, b = true }]),
    ?assertEqual([1], Subs),
    ok = erl_betree:betree_free(Betree).

-record(first, { a }).
-record(second, { b }).
atom_multiple_domain_list_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    Domains = [
               [ {a, bool, disallow_undefined} ], 
               [ {b, bool, disallow_undefined} ]
              ],
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, <<"a and b">>)),
    {ok, Subs} = erl_betree:betree_search_with_term(Betree, [#first{ a = true },#second{ b = true }]),
    ?assertEqual([1], Subs),
    ok = erl_betree:betree_free(Betree).

-record(all, { b, i, f, s, il, sl, seg, freq, now }).
atom_all_search_term_test() ->
    {ok, Betree} = erl_betree:betree_make(),
    Domains = [[
                {b, bool, disallow_undefined}, 
                {i, int, disallow_undefined}, 
                {f, float, disallow_undefined}, 
                {s, bin, disallow_undefined}, 
                {il, int_list, disallow_undefined}, 
                {sl, bin_list, disallow_undefined}, 
                {seg, segments, disallow_undefined}, 
                {frequency_caps, frequency_caps, disallow_undefined},
                {now, int64, disallow_undefined}
               ]],
    ok = erl_betree:betree_add_domains(Betree, Domains),
    Expr = <<
             "b and "
             "i = 10 and "
             "f > 3.13 and "
             "s = \"good\" and "
             "1 in il and "
             "sl none of (\"good\") and "
             "segment_within(seg, 1, 20) and ",
             "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
           >>,
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, Expr)),
    Usec = 1000 * 1000,
    Event = #all{
               b = true,
               i = 10,
               f = 3.14,
               s = <<"good">>,
               il = [1, 2, 3],
               sl = [<<"bad">>],
               seg = [{1, 10 * Usec}],
               freq = [{{<<"flight">>, 10, <<"ns">>}, 0, undefined}],
               now = 0
              },
    {ok, Subs} = erl_betree:betree_search_with_term(Betree, [Event]),
    ?assertEqual([1], Subs),
    ok = erl_betree:betree_free(Betree).

