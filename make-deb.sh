#!/bin/bash

set -e

DIR=.make-deb
TAG=`git describe --tags` # will fail if there are no tags

stack build
rm -rf "$DIR"
mkdir "$DIR"
cd "$DIR"
cp  `stack path --local-install-root`/bin/foundationdb-explorer .

fpm --input-type dir \
  --output-type deb \
  --version "$TAG" \
  --name foundationdb-explorer \
  --description 'Basic FoundationDB database browser' \
  --vendor '' \
  --maintainer '' \
  --url "https://github.com/Dretch/foundationdb-haskell" \
  --depends libgtk-3-0 \
  --depends libpangocairo-1.0-0 \
  --depends libpango-1.0-0 \
  --depends libatk1.0-0 \
  --depends libcairo-gobject2 \
  --depends libgdk-pixbuf2.0-0 \
  --depends libglib2.0-0 \
  --depends libgirepository-1.0-1 \
  --depends zlib1g \
  --depends foundationdb-clients \
  --depends libc6 \
  --depends libgmp10 \
  --prefix /usr/bin \
  foundationdb-explorer
