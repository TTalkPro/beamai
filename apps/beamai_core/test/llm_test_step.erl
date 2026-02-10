%%%-------------------------------------------------------------------
%%% @doc LLM test step module for integration tests
%%%
%%% Implements beamai_step_behaviour with 3 step types:
%%% - llm_call: Calls LLM with input prompt, emits response event
%%% - llm_transform: Takes text input, sends to LLM for transformation
%%% - llm_pause: Calls LLM, then pauses for human review
%%% @end
%%%-------------------------------------------------------------------
-module(llm_test_step).

-behaviour(beamai_step_behaviour).

-export([init/1, can_activate/2, on_activate/3, on_resume/3]).

%%====================================================================
%% llm_call — 调用 LLM 并发出响应事件
%%====================================================================

init(#{type := llm_call} = Config) ->
    LlmConfig = maps:get(llm_config, Config),
    OutputEvent = maps:get(output_event, Config, llm_response),
    PromptTemplate = maps:get(prompt_template, Config, <<"You are a helpful assistant.">>),
    {ok, #{type => llm_call,
           llm_config => LlmConfig,
           output_event => OutputEvent,
           prompt_template => PromptTemplate,
           response => undefined}};

%%====================================================================
%% llm_transform — 对输入文本进行 LLM 转换
%%====================================================================

init(#{type := llm_transform} = Config) ->
    LlmConfig = maps:get(llm_config, Config),
    OutputEvent = maps:get(output_event, Config, llm_transformed),
    Instruction = maps:get(instruction, Config, <<"Summarize the following text:">>),
    {ok, #{type => llm_transform,
           llm_config => LlmConfig,
           output_event => OutputEvent,
           instruction => Instruction,
           response => undefined}};

%%====================================================================
%% llm_pause — 调用 LLM 后暂停等待人工审核
%%====================================================================

init(#{type := llm_pause} = Config) ->
    LlmConfig = maps:get(llm_config, Config),
    OutputEvent = maps:get(output_event, Config, llm_approved),
    {ok, #{type => llm_pause,
           llm_config => LlmConfig,
           output_event => OutputEvent,
           response => undefined,
           resumed => false,
           approval => undefined}}.

%%====================================================================
%% can_activate
%%====================================================================

can_activate(_Inputs, _State) ->
    true.

%%====================================================================
%% on_activate — llm_call
%%====================================================================

on_activate(Inputs, #{type := llm_call, llm_config := LlmConfig,
                      output_event := OutputEvent,
                      prompt_template := PromptTemplate} = State, _Context) ->
    Prompt = extract_prompt(Inputs, PromptTemplate),
    case call_llm(LlmConfig, Prompt) of
        {ok, Response} ->
            Event = beamai_process_event:new(OutputEvent, #{
                prompt => Prompt,
                response => Response
            }),
            {ok, #{events => [Event],
                   state => State#{response => Response}}};
        {error, Reason} ->
            {error, {llm_call_failed, Reason}}
    end;

%%====================================================================
%% on_activate — llm_transform
%%====================================================================

on_activate(Inputs, #{type := llm_transform, llm_config := LlmConfig,
                      output_event := OutputEvent,
                      instruction := Instruction} = State, _Context) ->
    InputText = extract_text(Inputs),
    Prompt = <<Instruction/binary, "\n\n", InputText/binary>>,
    case call_llm(LlmConfig, Prompt) of
        {ok, Response} ->
            Event = beamai_process_event:new(OutputEvent, #{
                input_text => InputText,
                transformed => Response
            }),
            {ok, #{events => [Event],
                   state => State#{response => Response}}};
        {error, Reason} ->
            {error, {llm_transform_failed, Reason}}
    end;

%%====================================================================
%% on_activate — llm_pause
%%====================================================================

on_activate(_Inputs, #{type := llm_pause, resumed := true,
                       output_event := OutputEvent} = State, _Context) ->
    Event = beamai_process_event:new(OutputEvent, #{
        response => maps:get(response, State),
        approval => maps:get(approval, State)
    }),
    {ok, #{events => [Event], state => State}};

on_activate(Inputs, #{type := llm_pause, llm_config := LlmConfig,
                      resumed := false} = State, _Context) ->
    Prompt = extract_prompt(Inputs, <<"Please analyze the following request.">>),
    case call_llm(LlmConfig, Prompt) of
        {ok, Response} ->
            {pause, {awaiting_review, Response},
             State#{response => Response}};
        {error, Reason} ->
            {error, {llm_pause_call_failed, Reason}}
    end.

%%====================================================================
%% on_resume — llm_pause
%%====================================================================

on_resume(Data, #{type := llm_pause} = State, _Context) ->
    {ok, #{events => [],
           state => State#{resumed => true, approval => Data}}}.

%%====================================================================
%% Internal helpers
%%====================================================================

call_llm(LlmConfig, Prompt) ->
    Messages = [#{role => user,
                  content => Prompt}],
    case beamai_chat_completion:chat(LlmConfig, Messages) of
        {ok, #{content := Content}} when is_binary(Content) ->
            {ok, Content};
        {ok, #{content := null}} ->
            {ok, <<>>};
        {error, Reason} ->
            {error, Reason}
    end.

extract_prompt(#{input := #{prompt := Prompt}}, _Default) when is_binary(Prompt) ->
    Prompt;
extract_prompt(#{input := #{<<"prompt">> := Prompt}}, _Default) when is_binary(Prompt) ->
    Prompt;
extract_prompt(#{input := Prompt}, _Default) when is_binary(Prompt) ->
    Prompt;
extract_prompt(_, Default) ->
    Default.

extract_text(#{input := #{text := Text}}) when is_binary(Text) ->
    Text;
extract_text(#{input := #{response := Text}}) when is_binary(Text) ->
    Text;
extract_text(#{input := #{transformed := Text}}) when is_binary(Text) ->
    Text;
extract_text(#{input := Text}) when is_binary(Text) ->
    Text;
extract_text(_) ->
    <<"Hello, please respond with a brief greeting.">>.
