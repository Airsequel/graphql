#!/bin/sh

# setup
cabal update

# doc
cabal haddock --enable-documentation --with-compiler=/srv/httpd/server/ghcup/9.2/bin/ghc

# lint
cabal install hlint --with-compiler=/srv/httpd/server/ghcup/9.2/bin/ghc --overwrite-policy=always "--constraint=hlint ==3.5"
cabal exec hlint -- src tests

# test
cabal build graphql-test --with-compiler=/srv/httpd/server/ghcup/9.2/bin/ghc
cabal test --test-show-details=direct --with-compiler=/srv/httpd/server/ghcup/9.2/bin/ghc
