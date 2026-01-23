.PHONY: compile shell clean compile-graph shell-graph

ERL_LIBS := ../_build/default/lib

compile:
	ERL_LIBS=$(ERL_LIBS) rebar3 compile

shell:
	ERL_LIBS=$(ERL_LIBS) rebar3 shell

clean:
	rm -rf _build rebar.lock

# beamai_graph 独立编译 (无外部依赖)
compile-graph:
	cd ../apps/beamai_graph && rebar3 compile

# 启动包含 beamai_graph 的 shell
shell-graph: compile-graph
	ERL_LIBS=$(ERL_LIBS):../apps/beamai_graph/_build/default/lib rebar3 shell --apps beamai_graph
