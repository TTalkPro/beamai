%%%-------------------------------------------------------------------
%%% @doc Deep Agent 实时测试套件
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_live_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0, init_per_suite/1, end_per_suite/1]).

%% Test cases
-export([test_filesystem_exploration/1]).

all() ->
    [test_filesystem_exploration].

init_per_suite(Config) ->
    application:ensure_all_started(beamai_runtime),
    Config.

end_per_suite(_Config) ->
    ok.

test_filesystem_exploration(_Config) ->
    ct:pal("~n=== Deep Agent 文件系统探索测试 ===~n"),

    %% 获取 API Key
    ApiKey = case os:getenv("ZHIPU_API_KEY") of
        false ->
            ct:fail("ZHIPU_API_KEY 环境变量未设置");
        Key -> list_to_binary(Key)  %% 转换为 binary
    end,

    %% 配置 - 使用 GLM-4.6 + Zhipu Provider
    LLMConfig = llm_client:create(zhipu, #{
        model => <<"glm-4.6">>,
        api_key => ApiKey,
        timeout => 120000
    }),

    TargetRoot = <<"/home/david/workspace/research/clojure-in-actions">>,
    ct:pal("目标目录: ~s", [TargetRoot]),

    %% 创建配置 - 禁用计划，让 Agent 直接使用工具
    Config = beamai_deepagent:new(#{
        llm => LLMConfig,
        max_iterations => 30,
        max_depth => 2,
        planning_enabled => false,  %% 禁用计划，直接行动
        reflection_enabled => false,
        filesystem_enabled => true,
        filesystem => #{
            type => filesystem,
            root => TargetRoot
        },
        system_prompt => <<"你是软件工程师助手。使用 ls 和 read_file 工具探索项目。
每次只使用一个工具，分析结果后再决定下一步。
完成所有分析后，直接给出总结。"/utf8>>
    }),

    Task = <<"请按顺序完成以下任务：
1. 用 ls 命令查看 lib 目录下有什么项目
2. 进入发现的项目目录，用 ls 查看项目文件结构
3. 读取项目的 deps.edn 文件（注意使用完整路径如 lib/项目名/deps.edn）
4. 根据 deps.edn 内容，总结：项目类型、主要依赖、如何编译运行"/utf8>>,
    ct:pal("任务: ~s", [Task]),

    %% 运行 - 使用 Pregel 引擎
    ct:pal("开始执行 beamai_deepagent:run ..."),
    Result = beamai_deepagent:run(Config, Task),
    ct:pal("执行结果: ~p", [Result]),
    case Result of
        {ok, #{status := Status, response := Response}} ->
            ct:pal("状态: ~p", [Status]),
            ct:pal("响应:~n~s", [Response]),
            ok;
        {ok, Other} ->
            ct:pal("其他结果: ~p", [Other]),
            ok;
        {error, Reason} ->
            ct:pal("错误详情: ~p", [Reason]),
            ct:fail("执行失败: ~p", [Reason])
    end.
