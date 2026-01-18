%%%-------------------------------------------------------------------
%%% @doc LLM Mock 工具模块（仅用于测试）
%%%
%%% 此模块提供 Mock 实现用于单元测试和集成测试。
%%% 不应在生产代码中使用此模块。
%%%
%%% 功能:
%%%   - Mock 专家分析生成
%%%   - Mock 综合建议生成
%%%   - Mock 笑话生成
%%%   - 可配置延迟模拟
%%%   - 预设响应模板
%%%
%%% 使用示例:
%%% ```erlang
%%% %% 生成 Mock 技术分析
%%% Analysis = llm_mock_utils:tech_analysis(<<"问题">>).
%%%
%%% %% 生成 Mock 笑话
%%% Joke = llm_mock_utils:mock_joke(<<"cat">>).
%%%'
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_mock_utils).

%% API 导出
-export([tech_analysis/1, biz_analysis/1, ux_analysis/1]).
-export([security_analysis/1, data_analysis/1]).
-export([generic_analysis/2]).
-export([recommendation/1]).
-export([mock_joke/1]).
-export([simulate_delay/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type expert_id() :: atom().
-type mock_opts() :: #{
    delay => non_neg_integer(),      %% 模拟延迟 (毫秒)
    detail_level => low | medium | high,  %% 详细程度
    include_metrics => boolean()     %% 是否包含指标
}.

%%====================================================================
%% Mock 分析 API
%%====================================================================

%% @doc 生成 Mock 技术分析
-spec tech_analysis(binary()) -> binary().
tech_analysis(Input) ->
    iolist_to_binary([
        <<"## 技术分析报告\n\n"/utf8>>,
        <<"**分析问题**: "/utf8>>, truncate(Input, 100), <<"\n\n"/utf8>>,

        <<"### 1. 技术可行性评估\n"/utf8>>,
        <<"- **可行性等级**: 高\n"/utf8>>,
        <<"- **技术成熟度**: 成熟，有多种可选方案\n"/utf8>>,
        <<"- **实施复杂度**: 中等\n\n"/utf8>>,

        <<"### 2. 架构建议\n"/utf8>>,
        <<"- 建议采用微服务架构以支持独立部署和扩展\n"/utf8>>,
        <<"- 使用消息队列实现服务间异步通信\n"/utf8>>,
        <<"- 考虑容器化部署提高运维效率\n\n"/utf8>>,

        <<"### 3. 性能考量\n"/utf8>>,
        <<"- 预期 QPS: 1000-5000\n"/utf8>>,
        <<"- 响应时间目标: P99 < 200ms\n"/utf8>>,
        <<"- 建议使用缓存层优化热点数据访问\n\n"/utf8>>,

        <<"### 4. 技术风险\n"/utf8>>,
        <<"- **风险等级**: 中\n"/utf8>>,
        <<"- 主要风险: 系统集成复杂度、数据一致性\n"/utf8>>,
        <<"- 缓解措施: 原型验证、渐进式迁移\n"/utf8>>
    ]).

%% @doc 生成 Mock 业务分析
-spec biz_analysis(binary()) -> binary().
biz_analysis(Input) ->
    iolist_to_binary([
        <<"## 业务分析报告\n\n"/utf8>>,
        <<"**分析问题**: "/utf8>>, truncate(Input, 100), <<"\n\n"/utf8>>,

        <<"### 1. 市场机会评估\n"/utf8>>,
        <<"- **市场规模**: 中大型，增长潜力显著\n"/utf8>>,
        <<"- **竞争态势**: 存在竞品但有差异化空间\n"/utf8>>,
        <<"- **进入时机**: 适中，建议尽快布局\n\n"/utf8>>,

        <<"### 2. 成本效益分析\n"/utf8>>,
        <<"- 预计初期投入: 中等\n"/utf8>>,
        <<"- ROI 预期: 12-18个月回本\n"/utf8>>,
        <<"- 长期价值: 高，可形成竞争壁垒\n\n"/utf8>>,

        <<"### 3. 商业模式建议\n"/utf8>>,
        <<"- 采用 SaaS 订阅模式\n"/utf8>>,
        <<"- 分层定价满足不同客户需求\n"/utf8>>,
        <<"- 考虑增值服务提升 ARPU\n\n"/utf8>>,

        <<"### 4. 风险与应对\n"/utf8>>,
        <<"- **商业风险**: 低至中等\n"/utf8>>,
        <<"- 主要挑战: 市场教育、客户获取成本\n"/utf8>>,
        <<"- 建议策略: 聚焦核心场景，快速迭代验证\n"/utf8>>
    ]).

%% @doc 生成 Mock UX 分析
-spec ux_analysis(binary()) -> binary().
ux_analysis(Input) ->
    iolist_to_binary([
        <<"## 用户体验分析报告\n\n"/utf8>>,
        <<"**分析问题**: "/utf8>>, truncate(Input, 100), <<"\n\n"/utf8>>,

        <<"### 1. 用户需求洞察\n"/utf8>>,
        <<"- **核心需求**: 简单高效完成任务\n"/utf8>>,
        <<"- **痛点分析**: 现有方案操作复杂、学习成本高\n"/utf8>>,
        <<"- **期望体验**: 直觉式交互、即时反馈\n\n"/utf8>>,

        <<"### 2. 用户旅程设计\n"/utf8>>,
        <<"- 简化注册/登录流程\n"/utf8>>,
        <<"- 提供新手引导和上下文帮助\n"/utf8>>,
        <<"- 设计渐进式功能发现机制\n\n"/utf8>>,

        <<"### 3. 界面设计建议\n"/utf8>>,
        <<"- 采用简洁现代的视觉风格\n"/utf8>>,
        <<"- 保持交互一致性\n"/utf8>>,
        <<"- 支持响应式布局适配多终端\n\n"/utf8>>,

        <<"### 4. 可访问性考量\n"/utf8>>,
        <<"- 遵循 WCAG 2.1 AA 标准\n"/utf8>>,
        <<"- 支持键盘导航和屏幕阅读器\n"/utf8>>,
        <<"- 提供高对比度模式选项\n"/utf8>>
    ]).

%% @doc 生成 Mock 安全分析
-spec security_analysis(binary()) -> binary().
security_analysis(Input) ->
    iolist_to_binary([
        <<"## 安全分析报告\n\n"/utf8>>,
        <<"**分析问题**: "/utf8>>, truncate(Input, 100), <<"\n\n"/utf8>>,

        <<"### 1. 安全威胁评估\n"/utf8>>,
        <<"- **威胁等级**: 中等\n"/utf8>>,
        <<"- 主要威胁: 数据泄露、未授权访问、注入攻击\n"/utf8>>,
        <<"- 攻击面: API 接口、用户输入、第三方集成\n\n"/utf8>>,

        <<"### 2. 安全架构建议\n"/utf8>>,
        <<"- 实施零信任安全模型\n"/utf8>>,
        <<"- 加密敏感数据（静态和传输中）\n"/utf8>>,
        <<"- 建立完善的日志和审计机制\n\n"/utf8>>,

        <<"### 3. 合规要求\n"/utf8>>,
        <<"- 符合 GDPR 数据保护要求\n"/utf8>>,
        <<"- 满足行业安全标准\n"/utf8>>,
        <<"- 定期安全审计和渗透测试\n"/utf8>>
    ]).

%% @doc 生成 Mock 数据分析
-spec data_analysis(binary()) -> binary().
data_analysis(Input) ->
    iolist_to_binary([
        <<"## 数据分析报告\n\n"/utf8>>,
        <<"**分析问题**: "/utf8>>, truncate(Input, 100), <<"\n\n"/utf8>>,

        <<"### 1. 数据需求分析\n"/utf8>>,
        <<"- 核心数据实体识别\n"/utf8>>,
        <<"- 数据来源和采集方案\n"/utf8>>,
        <<"- 数据质量要求\n\n"/utf8>>,

        <<"### 2. 数据架构建议\n"/utf8>>,
        <<"- 采用数据湖架构支持多样化分析\n"/utf8>>,
        <<"- 实施数据治理框架\n"/utf8>>,
        <<"- 建立元数据管理系统\n\n"/utf8>>,

        <<"### 3. 分析能力规划\n"/utf8>>,
        <<"- 实时分析: 使用流处理引擎\n"/utf8>>,
        <<"- 批量分析: 使用分布式计算框架\n"/utf8>>,
        <<"- 可视化: 建立自助分析平台\n"/utf8>>
    ]).

%% @doc 生成通用分析
-spec generic_analysis(expert_id(), binary()) -> binary().
generic_analysis(ExpertId, Input) ->
    ExpertName = atom_to_binary(ExpertId),
    iolist_to_binary([
        <<"## "/utf8>>, ExpertName, <<" 分析报告\n\n"/utf8>>,
        <<"**分析问题**: "/utf8>>, truncate(Input, 100), <<"\n\n"/utf8>>,

        <<"### 分析结论\n"/utf8>>,
        <<"基于 "/utf8>>, ExpertName, <<" 的专业视角，该问题具有可行性。\n\n"/utf8>>,

        <<"### 建议\n"/utf8>>,
        <<"1. 进一步细化需求\n"/utf8>>,
        <<"2. 制定详细实施计划\n"/utf8>>,
        <<"3. 建立风险监控机制\n"/utf8>>
    ]).

%% @doc 生成 Mock 综合建议
-spec recommendation([{expert_id(), binary()}]) -> binary().
recommendation(Analyses) ->
    ExpertSummaries = lists:map(fun({ExpertId, Analysis}) ->
        ExpertName = expert_display_name(ExpertId),
        Summary = extract_summary(Analysis),
        [<<"- **"/utf8>>, ExpertName, <<"**: "/utf8>>, Summary, <<"\n"/utf8>>]
    end, Analyses),

    iolist_to_binary([
        <<"## 综合建议报告\n\n"/utf8>>,

        <<"### 专家意见汇总\n"/utf8>>,
        ExpertSummaries,
        <<"\n"/utf8>>,

        <<"### 综合评估\n"/utf8>>,
        <<"基于多位专家的分析，该方案整体可行，建议推进实施。\n\n"/utf8>>,

        <<"### 实施建议\n"/utf8>>,
        <<"1. **短期** (1-3个月): 完成原型开发和验证\n"/utf8>>,
        <<"2. **中期** (3-6个月): 核心功能开发和内测\n"/utf8>>,
        <<"3. **长期** (6-12个月): 全面上线和持续优化\n\n"/utf8>>,

        <<"### 风险提示\n"/utf8>>,
        <<"- 需关注技术实施风险\n"/utf8>>,
        <<"- 持续监控市场变化\n"/utf8>>,
        <<"- 重视用户反馈迭代\n\n"/utf8>>,

        <<"### 决策建议\n"/utf8>>,
        <<"**建议**: 批准启动，分阶段推进实施。\n"/utf8>>
    ]).

%%====================================================================
%% Mock 笑话生成（用于测试）
%%====================================================================

%% @doc 生成 Mock 笑话
-spec mock_joke(binary()) -> binary().
mock_joke(<<"cat">>) ->
    <<"Why don't cats play poker in the jungle? Too many cheetahs!">>;
mock_joke(<<"dog">>) ->
    <<"Why do dogs run in circles? It's hard to run in squares!">>;
mock_joke(<<"programmer">>) ->
    <<"Why do programmers prefer dark mode? Because light attracts bugs!">>;
mock_joke(<<"AI">>) ->
    <<"Why did the AI go to therapy? It had too many neural issues!">>;
mock_joke(<<"coffee">>) ->
    <<"Why do programmers drink coffee? They need Java!">>;
mock_joke(<<"recursion">>) ->
    <<"To understand recursion, you must first understand recursion.">>;
mock_joke(Topic) ->
    iolist_to_binary([
        <<"Why did ">>, Topic, <<" cross the road? To get to the other function!">>
    ]).

%% @doc 模拟处理延迟
-spec simulate_delay(mock_opts()) -> ok.
simulate_delay(#{delay := Delay}) when Delay > 0 ->
    timer:sleep(Delay),
    ok;
simulate_delay(_) ->
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 截断文本
-spec truncate(binary(), non_neg_integer()) -> binary().
truncate(Text, MaxLen) when byte_size(Text) > MaxLen ->
    <<Truncated:MaxLen/binary, _/binary>> = Text,
    <<Truncated/binary, <<"..."/utf8>>/binary>>;
truncate(Text, _MaxLen) ->
    Text.

%% @private 获取专家显示名称
-spec expert_display_name(expert_id()) -> binary().
expert_display_name(tech_expert) -> <<"技术专家"/utf8>>;
expert_display_name(biz_expert) -> <<"业务专家"/utf8>>;
expert_display_name(ux_expert) -> <<"用户体验专家"/utf8>>;
expert_display_name(security_expert) -> <<"安全专家"/utf8>>;
expert_display_name(data_expert) -> <<"数据专家"/utf8>>;
expert_display_name(ExpertId) -> atom_to_binary(ExpertId).

%% @private 从分析中提取摘要
-spec extract_summary(binary()) -> binary().
extract_summary(Analysis) ->
    %% 简单提取前 80 字符作为摘要
    case binary:match(Analysis, <<"\n">>) of
        {Pos, _} when Pos < 80 ->
            binary:part(Analysis, 0, Pos);
        _ ->
            truncate(Analysis, 80)
    end.
