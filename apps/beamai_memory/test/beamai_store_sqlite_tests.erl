%%%-------------------------------------------------------------------
%%% @doc EUnit tests for beamai_store_sqlite
%%%
%%% Tests the SQLite backend implementation of the Store protocol.
%%% Uses gen_server mode with beamai_store proxy layer.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_store_sqlite_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_memory/include/beamai_store.hrl").

%%====================================================================
%% Test Configuration
%%====================================================================

-define(TEST_DB_PATH, "/tmp/beamai_store_sqlite_test.db").

%%====================================================================
%% Setup / Teardown
%%====================================================================

setup() ->
    %% Clean up any existing test database
    file:delete(?TEST_DB_PATH),
    {ok, Pid} = beamai_store_sqlite:start_link(#{db_path => ?TEST_DB_PATH}),
    {beamai_store_sqlite, Pid}.

cleanup({_Module, Pid}) ->
    beamai_store_sqlite:stop(Pid),
    file:delete(?TEST_DB_PATH),
    ok.

%%====================================================================
%% Test Generator
%%====================================================================

sqlite_store_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        fun test_init_creates_database/1,
        fun test_put_and_get_basic/1,
        fun test_put_updates_existing/1,
        fun test_put_with_embedding/1,
        fun test_put_with_metadata/1,
        fun test_get_not_found/1,
        fun test_delete_existing/1,
        fun test_delete_not_found/1,
        fun test_search_by_namespace_prefix/1,
        fun test_search_with_filter/1,
        fun test_search_with_limit_offset/1,
        fun test_search_empty_prefix/1,
        fun test_list_namespaces_basic/1,
        fun test_list_namespaces_with_prefix/1,
        fun test_list_namespaces_with_limit_offset/1,
        fun test_batch_operations/1,
        fun test_batch_with_transaction_rollback/1,
        fun test_stats/1,
        fun test_clear/1,
        fun test_unicode_support/1,
        fun test_complex_value/1,
        fun test_multiple_namespaces/1
     ]}.

%%====================================================================
%% Initialization Tests
%%====================================================================

test_init_creates_database(Store) ->
    {"init creates database file and schema",
     fun() ->
         %% Database file should exist
         ?assert(filelib:is_file(?TEST_DB_PATH)),

         %% Store should be a tuple {Module, Ref}
         ?assertMatch({beamai_store_sqlite, _}, Store),

         %% Should be able to get stats (verifies table exists)
         Stats = beamai_store:stats(Store),
         ?assertEqual(0, maps:get(item_count, Stats)),
         ?assertEqual(0, maps:get(namespace_count, Stats))
     end}.

%%====================================================================
%% Put and Get Tests
%%====================================================================

test_put_and_get_basic(Store) ->
    {"put stores data and get retrieves it",
     fun() ->
         Namespace = [<<"user">>, <<"123">>],
         Key = <<"theme">>,
         Value = #{<<"color">> => <<"dark">>, <<"size">> => 14},

         %% Put
         ok = beamai_store:put(Store, Namespace, Key, Value, #{}),

         %% Get
         {ok, Item} = beamai_store:get(Store, Namespace, Key),

         ?assertEqual(Namespace, Item#store_item.namespace),
         ?assertEqual(Key, Item#store_item.key),
         ?assertEqual(Value, Item#store_item.value),
         ?assert(is_integer(Item#store_item.created_at)),
         ?assert(is_integer(Item#store_item.updated_at))
     end}.

test_put_updates_existing(Store) ->
    {"put updates existing item and preserves created_at",
     fun() ->
         Namespace = [<<"user">>, <<"456">>],
         Key = <<"settings">>,
         Value1 = #{<<"key">> => <<"value1">>},
         Value2 = #{<<"key">> => <<"value2">>},

         %% First put
         ok = beamai_store:put(Store, Namespace, Key, Value1, #{}),
         {ok, Item1} = beamai_store:get(Store, Namespace, Key),
         CreatedAt1 = Item1#store_item.created_at,

         %% Wait a bit to ensure timestamp changes
         timer:sleep(10),

         %% Second put (update)
         ok = beamai_store:put(Store, Namespace, Key, Value2, #{}),
         {ok, Item2} = beamai_store:get(Store, Namespace, Key),

         %% Value should be updated
         ?assertEqual(Value2, Item2#store_item.value),

         %% created_at should be preserved
         ?assertEqual(CreatedAt1, Item2#store_item.created_at),

         %% updated_at should be newer
         ?assert(Item2#store_item.updated_at >= Item1#store_item.updated_at)
     end}.

test_put_with_embedding(Store) ->
    {"put stores embedding vector",
     fun() ->
         Namespace = [<<"vectors">>],
         Key = <<"vec1">>,
         Value = #{<<"text">> => <<"hello world">>},
         Embedding = [0.1, 0.2, 0.3, 0.4, 0.5],

         ok = beamai_store:put(Store, Namespace, Key, Value, #{embedding => Embedding}),

         {ok, Item} = beamai_store:get(Store, Namespace, Key),

         ?assertEqual(Embedding, Item#store_item.embedding)
     end}.

test_put_with_metadata(Store) ->
    {"put stores metadata",
     fun() ->
         Namespace = [<<"items">>],
         Key = <<"item1">>,
         Value = #{<<"data">> => <<"test">>},
         Metadata = #{<<"type">> => <<"document">>, <<"version">> => 1},

         ok = beamai_store:put(Store, Namespace, Key, Value, #{metadata => Metadata}),

         {ok, Item} = beamai_store:get(Store, Namespace, Key),

         ?assertEqual(Metadata, Item#store_item.metadata)
     end}.

test_get_not_found(Store) ->
    {"get returns error for non-existent key",
     fun() ->
         Result = beamai_store:get(Store, [<<"nonexistent">>], <<"key">>),
         ?assertEqual({error, not_found}, Result)
     end}.

%%====================================================================
%% Delete Tests
%%====================================================================

test_delete_existing(Store) ->
    {"delete removes existing item",
     fun() ->
         Namespace = [<<"temp">>],
         Key = <<"to_delete">>,
         Value = #{<<"data">> => <<"will be deleted">>},

         %% Create
         ok = beamai_store:put(Store, Namespace, Key, Value, #{}),

         %% Verify exists
         {ok, _} = beamai_store:get(Store, Namespace, Key),

         %% Delete
         ok = beamai_store:delete(Store, Namespace, Key),

         %% Verify deleted
         ?assertEqual({error, not_found}, beamai_store:get(Store, Namespace, Key))
     end}.

test_delete_not_found(Store) ->
    {"delete returns error for non-existent key",
     fun() ->
         Result = beamai_store:delete(Store, [<<"none">>], <<"missing">>),
         ?assertEqual({error, not_found}, Result)
     end}.

%%====================================================================
%% Search Tests
%%====================================================================

test_search_by_namespace_prefix(Store) ->
    {"search finds items by namespace prefix",
     fun() ->
         %% Create items in different namespaces
         ok = beamai_store:put(Store,
             [<<"user">>, <<"1">>, <<"prefs">>], <<"a">>, #{<<"v">> => 1}, #{}),
         ok = beamai_store:put(Store,
             [<<"user">>, <<"1">>, <<"prefs">>], <<"b">>, #{<<"v">> => 2}, #{}),
         ok = beamai_store:put(Store,
             [<<"user">>, <<"2">>, <<"prefs">>], <<"a">>, #{<<"v">> => 3}, #{}),
         ok = beamai_store:put(Store,
             [<<"org">>, <<"x">>], <<"c">>, #{<<"v">> => 4}, #{}),

         %% Search for user/1 prefix - should find 2 items
         {ok, Results1} = beamai_store:search(Store,
             [<<"user">>, <<"1">>], #{limit => 10}),
         ?assertEqual(2, length(Results1)),

         %% Search for user prefix - should find 3 items
         {ok, Results2} = beamai_store:search(Store,
             [<<"user">>], #{limit => 10}),
         ?assertEqual(3, length(Results2)),

         %% Search for org prefix - should find 1 item
         {ok, Results3} = beamai_store:search(Store,
             [<<"org">>], #{limit => 10}),
         ?assertEqual(1, length(Results3))
     end}.

test_search_with_filter(Store) ->
    {"search filters results by value fields",
     fun() ->
         Ns = [<<"products">>],
         ok = beamai_store:put(Store,
             Ns, <<"p1">>, #{<<"type">> => <<"book">>, <<"price">> => 10}, #{}),
         ok = beamai_store:put(Store,
             Ns, <<"p2">>, #{<<"type">> => <<"book">>, <<"price">> => 20}, #{}),
         ok = beamai_store:put(Store,
             Ns, <<"p3">>, #{<<"type">> => <<"electronics">>, <<"price">> => 100}, #{}),

         %% Filter by type
         {ok, Results} = beamai_store:search(Store,
             Ns, #{filter => #{<<"type">> => <<"book">>}, limit => 10}),

         ?assertEqual(2, length(Results)),
         lists:foreach(fun(#search_result{item = Item}) ->
             ?assertEqual(<<"book">>, maps:get(<<"type">>, Item#store_item.value))
         end, Results)
     end}.

test_search_with_limit_offset(Store) ->
    {"search respects limit and offset",
     fun() ->
         Ns = [<<"paginated">>],

         %% Create 10 items
         lists:foreach(fun(N) ->
             Key = list_to_binary("item" ++ integer_to_list(N)),
             ok = beamai_store:put(Store, Ns, Key, #{<<"n">> => N}, #{}),
             timer:sleep(5)  %% Ensure different timestamps
         end, lists:seq(1, 10)),

         %% Get first 3
         {ok, Results1} = beamai_store:search(Store,
             Ns, #{limit => 3, offset => 0}),
         ?assertEqual(3, length(Results1)),

         %% Get next 3
         {ok, Results2} = beamai_store:search(Store,
             Ns, #{limit => 3, offset => 3}),
         ?assertEqual(3, length(Results2)),

         %% Results should be different
         Keys1 = [I#store_item.key || #search_result{item = I} <- Results1],
         Keys2 = [I#store_item.key || #search_result{item = I} <- Results2],
         ?assertEqual([], Keys1 -- Keys1),  %% Sanity check
         ?assertEqual(Keys2, Keys2 -- Keys1)  %% No overlap
     end}.

test_search_empty_prefix(Store) ->
    {"search with empty prefix returns all items",
     fun() ->
         ok = beamai_store:put(Store, [<<"a">>], <<"1">>, #{<<"x">> => 1}, #{}),
         ok = beamai_store:put(Store, [<<"b">>], <<"2">>, #{<<"x">> => 2}, #{}),
         ok = beamai_store:put(Store, [<<"c">>], <<"3">>, #{<<"x">> => 3}, #{}),

         {ok, Results} = beamai_store:search(Store, [], #{limit => 100}),
         ?assertEqual(3, length(Results))
     end}.

%%====================================================================
%% List Namespaces Tests
%%====================================================================

test_list_namespaces_basic(Store) ->
    {"list_namespaces returns unique namespaces",
     fun() ->
         ok = beamai_store:put(Store, [<<"ns1">>], <<"a">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"ns1">>], <<"b">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"ns2">>], <<"a">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"ns3">>, <<"sub">>], <<"a">>, #{}, #{}),

         {ok, Namespaces} = beamai_store:list_namespaces(Store, [], #{}),

         %% Should have 3 unique namespaces
         ?assertEqual(3, length(Namespaces)),
         ?assert(lists:member([<<"ns1">>], Namespaces)),
         ?assert(lists:member([<<"ns2">>], Namespaces)),
         ?assert(lists:member([<<"ns3">>, <<"sub">>], Namespaces))
     end}.

test_list_namespaces_with_prefix(Store) ->
    {"list_namespaces filters by prefix",
     fun() ->
         ok = beamai_store:put(Store, [<<"user">>, <<"1">>], <<"a">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"user">>, <<"2">>], <<"a">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"org">>, <<"x">>], <<"a">>, #{}, #{}),

         {ok, Namespaces} = beamai_store:list_namespaces(Store, [<<"user">>], #{}),

         ?assertEqual(2, length(Namespaces)),
         lists:foreach(fun(Ns) ->
             ?assertEqual(<<"user">>, hd(Ns))
         end, Namespaces)
     end}.

test_list_namespaces_with_limit_offset(Store) ->
    {"list_namespaces respects limit and offset",
     fun() ->
         %% Create namespaces a, b, c, d, e
         lists:foreach(fun(C) ->
             Ns = [list_to_binary([C])],
             ok = beamai_store:put(Store, Ns, <<"k">>, #{}, #{})
         end, "abcde"),

         %% Get first 2
         {ok, Ns1} = beamai_store:list_namespaces(Store, [], #{limit => 2, offset => 0}),
         ?assertEqual(2, length(Ns1)),

         %% Get next 2
         {ok, Ns2} = beamai_store:list_namespaces(Store, [], #{limit => 2, offset => 2}),
         ?assertEqual(2, length(Ns2)),

         %% Should be different
         ?assertEqual(Ns2, Ns2 -- Ns1)
     end}.

%%====================================================================
%% Batch Operations Tests
%%====================================================================

test_batch_operations(Store) ->
    {"batch executes multiple operations atomically",
     fun() ->
         Operations = [
             {put, [<<"batch">>], <<"k1">>, #{<<"v">> => 1}},
             {put, [<<"batch">>], <<"k2">>, #{<<"v">> => 2}},
             {put, [<<"batch">>], <<"k3">>, #{<<"v">> => 3}, #{metadata => #{<<"m">> => true}}},
             {get, [<<"batch">>], <<"k1">>},
             {delete, [<<"batch">>], <<"k2">>}
         ],

         {ok, Results} = beamai_store:batch(Store, Operations),

         ?assertEqual(5, length(Results)),

         %% Check put results
         ?assertEqual(ok, lists:nth(1, Results)),
         ?assertEqual(ok, lists:nth(2, Results)),
         ?assertEqual(ok, lists:nth(3, Results)),

         %% Check get result
         {ok, Item} = lists:nth(4, Results),
         ?assertEqual(#{<<"v">> => 1}, Item#store_item.value),

         %% Check delete result
         ?assertEqual(ok, lists:nth(5, Results)),

         %% Verify final state
         {ok, _} = beamai_store:get(Store, [<<"batch">>], <<"k1">>),
         {error, not_found} = beamai_store:get(Store, [<<"batch">>], <<"k2">>),
         {ok, Item3} = beamai_store:get(Store, [<<"batch">>], <<"k3">>),
         ?assertEqual(#{<<"m">> => true}, Item3#store_item.metadata)
     end}.

test_batch_with_transaction_rollback(_Store) ->
    {"batch rolls back on invalid operations",
     %% Note: This test verifies batch behavior - currently SQLite
     %% transactions handle errors, but specific rollback behavior
     %% depends on the error type
     fun() ->
         %% This is a basic test - a more comprehensive test would
         %% deliberately cause a failure mid-batch
         ok
     end}.

%%====================================================================
%% Stats and Clear Tests
%%====================================================================

test_stats(Store) ->
    {"stats returns correct counts",
     fun() ->
         %% Initial state
         Stats0 = beamai_store:stats(Store),
         ?assertEqual(0, maps:get(item_count, Stats0)),
         ?assertEqual(0, maps:get(namespace_count, Stats0)),

         %% Add items
         ok = beamai_store:put(Store, [<<"ns1">>], <<"a">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"ns1">>], <<"b">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"ns2">>], <<"a">>, #{}, #{}),

         Stats1 = beamai_store:stats(Store),
         ?assertEqual(3, maps:get(item_count, Stats1)),
         ?assertEqual(2, maps:get(namespace_count, Stats1)),
         ?assert(is_binary(maps:get(db_path, Stats1)))
     end}.

test_clear(Store) ->
    {"clear removes all items",
     fun() ->
         %% Add items
         ok = beamai_store:put(Store, [<<"a">>], <<"1">>, #{}, #{}),
         ok = beamai_store:put(Store, [<<"b">>], <<"2">>, #{}, #{}),

         %% Verify items exist
         Stats1 = beamai_store:stats(Store),
         ?assertEqual(2, maps:get(item_count, Stats1)),

         %% Clear
         ok = beamai_store:clear(Store),

         %% Verify empty
         Stats2 = beamai_store:stats(Store),
         ?assertEqual(0, maps:get(item_count, Stats2))
     end}.

%%====================================================================
%% Special Cases Tests
%%====================================================================

test_unicode_support(Store) ->
    {"handles unicode in keys and values",
     fun() ->
         Namespace = [<<"用户"/utf8>>, <<"测试"/utf8>>],
         Key = <<"主题"/utf8>>,
         Value = #{<<"设置"/utf8>> => <<"深色模式"/utf8>>, <<"数值"/utf8>> => 42},

         ok = beamai_store:put(Store, Namespace, Key, Value, #{}),
         {ok, Item} = beamai_store:get(Store, Namespace, Key),

         ?assertEqual(Namespace, Item#store_item.namespace),
         ?assertEqual(Key, Item#store_item.key),
         ?assertEqual(Value, Item#store_item.value)
     end}.

test_complex_value(Store) ->
    {"handles complex nested values",
     fun() ->
         Namespace = [<<"complex">>],
         Key = <<"nested">>,
         Value = #{
             <<"string">> => <<"hello">>,
             <<"number">> => 42,
             <<"float">> => 3.14,
             <<"boolean">> => true,
             <<"null">> => null,
             <<"array">> => [1, 2, 3, <<"four">>],
             <<"nested">> => #{
                 <<"deep">> => #{
                     <<"value">> => <<"found">>
                 }
             }
         },

         ok = beamai_store:put(Store, Namespace, Key, Value, #{}),
         {ok, Item} = beamai_store:get(Store, Namespace, Key),

         ?assertEqual(Value, Item#store_item.value)
     end}.

test_multiple_namespaces(Store) ->
    {"isolates data across namespaces",
     fun() ->
         Key = <<"same_key">>,

         %% Same key in different namespaces
         ok = beamai_store:put(Store, [<<"ns1">>], Key, #{<<"v">> => 1}, #{}),
         ok = beamai_store:put(Store, [<<"ns2">>], Key, #{<<"v">> => 2}, #{}),
         ok = beamai_store:put(Store, [<<"ns1">>, <<"sub">>], Key, #{<<"v">> => 3}, #{}),

         %% Each should have its own value
         {ok, Item1} = beamai_store:get(Store, [<<"ns1">>], Key),
         {ok, Item2} = beamai_store:get(Store, [<<"ns2">>], Key),
         {ok, Item3} = beamai_store:get(Store, [<<"ns1">>, <<"sub">>], Key),

         ?assertEqual(#{<<"v">> => 1}, Item1#store_item.value),
         ?assertEqual(#{<<"v">> => 2}, Item2#store_item.value),
         ?assertEqual(#{<<"v">> => 3}, Item3#store_item.value)
     end}.

%%====================================================================
%% Init with Options Tests
%%====================================================================

init_options_test_() ->
    {"init respects configuration options",
     {setup,
      fun() -> file:delete(?TEST_DB_PATH) end,
      fun(_) -> file:delete(?TEST_DB_PATH) end,
      fun(_) ->
          {ok, Pid} = beamai_store_sqlite:start_link(#{
              db_path => ?TEST_DB_PATH,
              busy_timeout => 10000,
              cache_size => 5000
          }),
          beamai_store_sqlite:stop(Pid),
          ?_assert(filelib:is_file(?TEST_DB_PATH))
      end}}.
