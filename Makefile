# all:

help:
	@echo "Usage:"
	@echo "make repl          # start a dev REPL"

repl:
	clj -M:nrepl:dev
.PHONY: repl
