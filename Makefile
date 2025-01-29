iced: ## Build the libiced_hs library only
	@cargo build

build: ## Build the rust library & haskell bindings
	@cabal build all

clean: ## Remove the rust & haskell build artifacts
	@cargo clean
	@cabal clean

repl: ## Start a cabal REPL
	@cabal repl iced-hs

ghci: repl ## Start a cabal REPL (alias for `make repl`)

watch: ## Load the main library and reload on file change
	@ghcid --target iced-hs-server -l

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

UNAME := $(shell uname)

SHELL := /usr/bin/env bash

ifeq ($(UNAME), Darwin)
	PROCS := $(shell sysctl -n hw.logicalcpu)
else
	PROCS := $(shell nproc)
endif

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
