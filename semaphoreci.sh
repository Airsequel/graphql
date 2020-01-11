#!/bin/bash

STACK=$SEMAPHORE_CACHE_DIR/stack
export STACK_ROOT=$SEMAPHORE_CACHE_DIR/.stack

setup() {
	if [ ! -e "$STACK" ]
	then
		curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C $SEMAPHORE_CACHE_DIR '*/stack'
	fi
	if [ -e "$SEMAPHORE_CACHE_DIR/graphql.cabal" ]
	then
		cp -a $SEMAPHORE_CACHE_DIR/graphql.cabal graphql.cabal
	fi
	$STACK --no-terminal setup
	cp -a graphql.cabal $SEMAPHORE_CACHE_DIR/graphql.cabal
}

setup_test() {
	$STACK --no-terminal test --only-snapshot
}

test() {
	$STACK --no-terminal test --pedantic
}

test_docs() {
	$STACK --no-terminal ghc -- -Wall -Werror -fno-code docs/tutorial/tutorial.lhs
	$STACK --no-terminal haddock --no-haddock-deps
}

setup_lint() {
	$STACK --no-terminal install hlint
}

lint() {
	$STACK --no-terminal exec hlint -- src tests
}

$1
