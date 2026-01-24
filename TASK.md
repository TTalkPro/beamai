# BeamAI Kernel - Task List

## Phase 1: Kernel + Plugin + Function

### Step 0: Documentation
- [x] Create `design/ARCHITECTURE.md`
- [x] Create `design/COMPARISON.md`
- [x] Create `TASK.md`
- [x] Create `TASK_DONE.md`

### Step 1: Project Structure
- [x] Create `apps/beamai_kernel/` directory structure
- [x] Create `beamai_kernel.app.src`
- [x] Create `apps/beamai_kernel/rebar.config`
- [x] Update main `rebar.config`

### Step 2: Core Data Structures
- [x] Implement `beamai_function.erl`
- [x] Implement `beamai_plugin.erl`
- [x] Implement `beamai_context.erl`
- [x] Implement `beamai_result.erl`

### Step 3: Kernel
- [x] Implement `beamai_kernel.erl`
- [x] Implement `beamai_filter.erl`

### Step 4: AI Service
- [x] Implement `beamai_service.erl`
- [x] Implement `beamai_chat_completion.erl`
- [x] Implement `beamai_prompt.erl`
- [x] Implement `beamai_connector.erl`

### Step 5: Connectors
- [x] Implement `beamai_connector_openai.erl`
- [x] Implement `beamai_connector_anthropic.erl`
- [x] Implement `beamai_connector_zhipu.erl`
- [x] Implement `beamai_connector_ollama.erl`

### Step 6: Tool Calling + Facade
- [x] Implement tool calling loop in `beamai_kernel.erl`
- [x] Implement `beamai.erl` facade

### Step 7: Tests
- [x] Implement `beamai_function_tests.erl`
- [x] Implement `beamai_plugin_tests.erl`
- [x] Implement `beamai_kernel_tests.erl`
- [x] Implement `beamai_filter_tests.erl`
- [x] Implement `beamai_chat_completion_tests.erl`

## Phase 2: Process Framework (Future)
- [ ] Design Step/Event model
- [ ] Implement beamai_process app

## Phase 3: Agent (Future)
- [ ] Design Agent = Kernel + Prompt + Memory
- [ ] Implement beamai_agent refactor

## Phase 4: Memory (Future)
- [ ] Design Semantic Memory as Service
- [ ] Implement memory service connector
