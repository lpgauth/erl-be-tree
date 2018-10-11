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

-record(bad_type_test, { bool }).
bad_type_test() ->
    Domains = [[
                {bool, bool, disallow_undefined}
               ]],
    Consts = [],
    Expr = <<"bool">>,
    Event = [#bad_type_test{bool = 1}],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, Consts, Expr)),
    ?assertError(badarg, erl_betree:betree_search(Betree, Event)),
    ok = erl_betree:betree_free(Betree).

-record(bad_frequency_caps, { l }).
bad_frequency_caps_test() ->
    Domains = [[
                {l, frequency_caps, disallow_undefined}
               ]],
    Consts = [],
    Expr = <<"true">>,
    Event = [#bad_frequency_caps{l = [10]}],
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = true}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = 10}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = 10.0}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = <<"a">>}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = [10]}], error),
    %[{Id, [], "true"}], Defs, [#fca_list_rec{l = [<<"a">>]}], error)
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, Consts, Expr)),
    ?assertError(badarg, erl_betree:betree_search(Betree, Event)),
    ok = erl_betree:betree_free(Betree).

-record(handle_undef_in_search, {def}).
handle_undef_in_search_test() ->
    Domains = [[
                {def, int, disallow_undefined}
               ]],
    Consts = [],
    Expr = <<"def = 10">>,
    Event = [#handle_undef_in_search{}],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, Consts, Expr)),
    ?assertError(badarg, erl_betree:betree_search(Betree, Event)),
    ok = erl_betree:betree_free(Betree).

-record(bug_geo_request,{exchange, member_id, latitude, longitude}).
-record(bug_geo_impression, {width, height, types}).
bug_geo_test() ->
    Domains = [[{exchange, int, disallow_undefined},
                {member_id, int, disallow_undefined},
                {latitude, float, allow_undefined},
                {longitude, float, allow_undefined}],
               [{width, int, disallow_undefined},
                {height, int, disallow_undefined},
                {types, int_list, disallow_undefined}]],
    Consts = [],
    Expr1 = <<"(((width is not null and width = 100) and (height is not null and height = 200) " 
              "and (types is not null and 1 in types) and true and true)) and (((exchange is not null and exchange = 2) " 
              "and (member_id is not null and member_id = 0) and true))">>,
    Expr2 = <<"(((width is not null and width = 100) and (height is not null and height = 200) " 
              "and (types is not null and 1 in types) and true and true)) and (((exchange is not null and exchange = 2) " 
              "and (member_id is not null and member_id = 0) and true)) and geo_within_radius(100.0, 100.0, 10.0)">>,
    Event = [#bug_geo_request{exchange = 2, member_id = 0, latitude = 100.0, longitude = 100.0},
             #bug_geo_impression{width = 100, height = 200, types = [1]}],
    {ok, Betree} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree, Domains),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 1, Consts, Expr1)),
    ?assertEqual(ok, erl_betree:betree_insert(Betree, 2, Consts, Expr2)),
    {ok, Subs} = erl_betree:betree_search(Betree, Event),
    ?assertEqual([1, 2], lists:sort(Subs)),
    ok = erl_betree:betree_free(Betree).

-record(multiple,{i}).
multiple_trees_test() ->
    Domains = [[{i, int, disallow_undefined}]],
    Consts = [],
    Expr1 = <<"i < 5">>,
    Expr2 = <<"i > 5">>,
    Event1 = [#multiple{i = 3}],
    Event2 = [#multiple{i = 8}],
    {ok, Betree1} = erl_betree:betree_make(),
    {ok, Betree2} = erl_betree:betree_make(),
    ok = erl_betree:betree_add_domains(Betree1, Domains),
    ok = erl_betree:betree_add_domains(Betree2, Domains),
    ok = erl_betree:betree_insert(Betree1, 1, Consts, Expr1),
    ok = erl_betree:betree_insert(Betree2, 2, Consts, Expr2),
    ?assertEqual({ok, [1]}, erl_betree:betree_search(Betree1, Event1)),
    ?assertEqual({ok, [ ]}, erl_betree:betree_search(Betree1, Event2)),
    ?assertEqual({ok, [ ]}, erl_betree:betree_search(Betree2, Event1)),
    ?assertEqual({ok, [2]}, erl_betree:betree_search(Betree2, Event2)),
    ok = erl_betree:betree_free(Betree2),
    ?assertEqual({ok, [1]}, erl_betree:betree_search(Betree1, Event1)),
    ?assertEqual({ok, [ ]}, erl_betree:betree_search(Betree1, Event2)),
    ok = erl_betree:betree_free(Betree1).

bad_domain_type_test() ->
    Domains = [[{i1, int, allow_undefined},
                {i2, innt, allow_undefined},
                {i3, int, allow_undefined}]],
    {ok, Betree} = erl_betree:betree_make(),
    ?assertError(badarg, erl_betree:betree_add_domains(Betree, Domains)),
    ok = erl_betree:betree_free(Betree).

bad_domain_undef_test() ->
    Domains = [[{i1, int, allow_undefined},
                {i2, int, alloww_undefined},
                {i3, int, allow_undefined}]],
    {ok, Betree} = erl_betree:betree_make(),
    ?assertError(badarg, erl_betree:betree_add_domains(Betree, Domains)),
    ok = erl_betree:betree_free(Betree).
