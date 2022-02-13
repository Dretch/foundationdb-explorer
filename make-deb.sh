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
cp ../icon.bmp ./usr/share/pixmaps/foundationdb-explorer.bmp

# lovely hack to find out what debian packages our executable needs
DEPENDS=$(dpkg --search `readelf -d ./usr/bin/foundationdb-explorer | egrep --only-matching '[^[]+\.so\.[^]]'` \
 | egrep --only-matching '^[^ :]+' \
 | sort \
 | uniq \
 | xargs -L 1 echo --depends)


fpm --input-type dir \
  --output-type deb \
  --version "$TAG" \
  --name foundationdb-explorer \
  --description 'Basic FoundationDB database browser' \
  --vendor '' \
  --maintainer '' \
  --url "https://github.com/Dretch/foundationdb-haskell" \
  $DEPENDS \
  .
