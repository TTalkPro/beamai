# BeamAI Examples Makefile
#
# 使用前请先编译主项目:
#   cd .. && rebar3 compile
#

BEAMAI_LIBS = ../_build/default/lib

.PHONY: compile shell clean

compile:
	ERL_LIBS=$(BEAMAI_LIBS) rebar3 compile

shell:
	ERL_LIBS=$(BEAMAI_LIBS) rebar3 shell

clean:
	rm -rf _build rebar.lock

help:
	@echo "BeamAI Examples"
	@echo ""
	@echo "使用前请先编译主项目:"
	@echo "  cd .. && rebar3 compile"
	@echo ""
	@echo "可用命令:"
	@echo "  make compile  - 编译 examples"
	@echo "  make shell    - 启动 Erlang shell"
	@echo "  make clean    - 清理构建文件"
