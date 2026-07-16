%%%-------------------------------------------------------------------
%%% @doc 工具错误分类：语义 / 瞬态 / 环境
%%%
%%% Agent loop 自带一个错误处理器——模型本身。工具失败按类别分层路由
%%% （见 design/hitl_timeline_serial_errors.md §3）：
%%%
%%%   - `semantic`（语义类，大多数）：参数错、查无此人、业务拒绝——序列化为
%%%     工具结果回给模型（errors are data），模型换参数/换工具/问用户。缺省路径。
%%%   - `transient`（瞬态类）：超时、429、网络抖动——工具级幂等重试（指数退避），
%%%     对模型透明。**硬前提：工具幂等**（opt-in retry 即承诺）。
%%%   - `environment`（环境类）：认证失效、配额烧光、磁盘满——模型修不了、重试
%%%     无用，屏障处带一致快照暂停等人。
%%%
%%% 判定顺序（全数据驱动，无 instanceof 动物园）：
%%%   1. 异常包装 `#{class, reason, stacktrace}`（beamai_tool 捕获形态）→ 递归 reason；
%%%   2. 显式 `error_class`（工具作者 `error(#{error_class => environment, ...})`
%%%      或返回 `{error, #{error_class => ...}}`）——最高优先级；
%%%   3. canonical llm_error（结构匹配 `#{'__llm_error__' := true, ...}`，不依赖
%%%      beamai_llm app）：`type=auth` → environment；`retryable=true` → transient；
%%%      其余 → semantic——工具内调 provider 的失败自动获正确路由，零标注；
%%%   4. Erlang 超时 / 网络（timeout / {request_failed,_} / {closed,_}）→ transient；
%%%   5. 缺省 semantic。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_error).

-export([classify/1, message/1]).

-export_type([class/0]).

-type class() :: semantic | transient | environment.

%%====================================================================
%% API
%%====================================================================

%% @doc 将工具错误 Reason 分类为 semantic | transient | environment
-spec classify(term()) -> class().
%% 1. 异常包装：beamai_tool call_handler 捕获为 #{class, reason, stacktrace}
classify(#{class := _, reason := R, stacktrace := _}) ->
    classify(R);
%% 2. 显式标注（最高优先级）
classify(#{error_class := C}) when C =:= semantic; C =:= transient; C =:= environment ->
    C;
%% 结构化工具错误 #{error => Inner}：递归看内层
classify(#{error := Inner}) when is_map(Inner) ->
    classify(Inner);
%% 3. canonical llm_error（结构匹配，不依赖 beamai_llm）
classify(#{'__llm_error__' := true, type := auth}) -> environment;
classify(#{'__llm_error__' := true, retryable := true}) -> transient;
classify(#{'__llm_error__' := true}) -> semantic;
%% 4. Erlang 超时 / 网络
classify(timeout) -> transient;
classify({timeout, _}) -> transient;
classify({request_failed, _}) -> transient;
classify({closed, _}) -> transient;
classify(tool_timeout) -> transient;
classify(tool_worker_crash) -> transient;
%% 批级 worker 崩溃（beamai_tool_batch_worker）：与 tool_worker_crash 同源——
%% 基础设施故障而非模型的语义错误，重试有意义
classify(tool_batch_crash) -> transient;
%% 5. 缺省语义类
classify(_) -> semantic.

%% @doc 尽量从错误 Reason 提取人类可读消息（binary）
-spec message(term()) -> binary().
message(#{class := _, reason := R, stacktrace := _}) -> message(R);
message(#{message := M}) when is_binary(M) -> M;
message(#{error := #{message := M}}) when is_binary(M) -> M;
message(B) when is_binary(B) -> B;
message(A) when is_atom(A) -> atom_to_binary(A, utf8);
message(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).
