#!/bin/bash

set -e

DIR=.make-deb
TAG=`git describe --tags` # will fail if there are no tags
TAG=${TAG#"v"} # remove "v" prefix

stack build
rm -rf "$DIR"
mkdir -p "$DIR/usr/bin" "$DIR/usr/share/applications" "$DIR/usr/share/pixmaps"
cd "$DIR"
cp `stack path --local-install-root`/bin/foundationdb-explorer ./usr/bin
cp ../foundationdb-explorer.desktop ./usr/share/applications
cp ../icon.png ./usr/share/pixmaps/foundationdb-explorer.png

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
  .
