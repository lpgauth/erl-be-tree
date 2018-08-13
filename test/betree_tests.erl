-module(betree_tests).

-include_lib("eunit/include/eunit.hrl").

-record(basic_test, { i }).
basic_test() ->
    Domains = [[{i, int, disallow_undefined, 0, 10}]],
    Expr = <<"i = 5">>,
    Event = [#basic_test{ i = 5 }],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ok = erl_betree:betree_insert(Betree, 1, [], Expr),
    {ok, Subs} = erl_betree:betree_search(Betree, Event),
    ?assertEqual(Subs, [1]),
    ok = erl_betree:betree_free(Betree).

bad_insert_no_domain_test() ->
    Expr = <<"i = 5">>,
    {ok, Betree} = erl_betree:betree_make(),
    ?assertEqual(error, erl_betree:betree_insert(Betree, 1, [], Expr)),
    ok = erl_betree:betree_free(Betree).

-record(bad_insert_bounded_string_test, { s }).
bad_insert_bounded_string_test() ->
    Domains = [[{s, bin, disallow_undefined, 1}]],
    Expr1 = <<"s <> \"good\"">>,
    Expr2 = <<"s <> \"bad\"">>,
    Event = [#bad_insert_bounded_string_test{ s = <<"diff">> }],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ok = erl_betree:betree_insert(Betree, 1, [], Expr1),
    ?assertEqual(error, erl_betree:betree_insert(Betree, 2, [], Expr2)),
    {ok, Subs} = erl_betree:betree_search(Betree, Event),
    ?assertEqual(Subs, [1]),
    ok = erl_betree:betree_free(Betree).

atom_all_domain_types_test() ->
    Domains = [[
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
               ]],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ok = erl_betree:betree_free(Betree).

-record(single, { a, b }).
atom_single_domain_list_test() ->
    Domains = [[
                {a, bool, disallow_undefined}, 
                {b, bool, disallow_undefined} 
               ]],
    Expr = <<"a and b">>,
    Event = [#single{ a = true, b = true }],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, [], Expr)),
    {ok, Subs} = erl_betree:betree_search(Betree, Event),
    ?assertEqual([1], Subs),
    ok = erl_betree:betree_free(Betree).

-record(first, { a }).
-record(second, { b }).
atom_multiple_domain_list_test() ->
    Domains = [
               [ {a, bool, disallow_undefined} ], 
               [ {b, bool, disallow_undefined} ]
              ],
    Expr = <<"a and b">>,
    Event = [#first{ a = true },#second{ b = true }],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, [], Expr)),
    {ok, Subs} = erl_betree:betree_search(Betree, Event),
    ?assertEqual([1], Subs),
    ok = erl_betree:betree_free(Betree).

-record(all, { b, i, f, s, il, sl, seg, freq, now }).
atom_all_search_term_test() ->
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
    Expr = <<
             "b and "
             "i = 10 and "
             "f > 3.13 and "
             "s = \"good\" and "
             "1 in il and "
             "sl none of (\"good\") and "
             "segment_within(seg, 1, 20) and "
             "within_frequency_cap(\"flight\", \"ns\", 100, 0)"
           >>,
    Consts = [{flight_id, 10},
              {advertiser_id, 20},
              {campaign_id, 30},
              {product_id, 40}],
    Usec = 1000 * 1000,
    Event = [#all{
               b = true,
               i = 10,
               f = 3.14,
               s = <<"good">>,
               il = [1, 2, 3],
               sl = [<<"bad">>],
               seg = [{1, 10 * Usec}],
               freq = [{{<<"flight">>, 10, <<"ns">>}, 0, undefined}],
               now = 0
              }],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, Consts, Expr)),
    {ok, Subs} = erl_betree:betree_search(Betree, Event),
    ?assertEqual([1], Subs),
    ok = erl_betree:betree_free(Betree).

-record(constant_test, { frequency_caps, now }).
constant_test() ->
    Domains = [[
                {frequency_caps, frequency_caps, disallow_undefined},
                {now, int64, disallow_undefined}
               ]],
    Consts = [{flight_id, 10},
              {advertiser_id, 20},
              {campaign_id, 30},
              {product_id, 40}],
    Expr = <<"within_frequency_cap(\"flight\", \"ns\", 100, 0)">>,
    Event = [#constant_test{ 
               frequency_caps = [{{<<"flight">>, 10, <<"ns">>}, 100, undefined}],
               now = 0
              }],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(error, erl_betree:betree_insert(Betree, 1, [], Expr)),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, Consts, Expr)),
    {ok, Subs} = erl_betree:betree_search(Betree, Event),
    ?assertEqual([], Subs),
    ok = erl_betree:betree_free(Betree).

