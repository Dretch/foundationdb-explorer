#!/bin/bash

set -ex

DIR=.make-deb
TAG=`git describe --tags` # will fail if there are no tags
TAG=${TAG#"v"} # remove "v" prefix

stack build
rm -rf "$DIR"
mkdir -p "$DIR/usr/bin" "$DIR/usr/share/applications" "$DIR/usr/share/pixmaps" "$DIR/usr/share/foundationdb-explorer" # todo: delete unused fonts!
cd "$DIR"
cp `stack path --local-install-root`/bin/foundationdb-explorer ./usr/share/foundationdb-explorer/
cp ../foundationdb-explorer.desktop ./usr/share/applications
cp ../assets/icon.bmp ./usr/share/pixmaps/foundationdb-explorer.bmp
cp -r ../assets ./usr/share/foundationdb-explorer

# change working directory when running so that assets can be found
echo 'cd /usr/share/foundationdb-explorer && exec /usr/share/foundationdb-explorer/foundationdb-explorer' > ./usr/bin/foundationdb-explorer
chmod +x ./usr/bin/foundationdb-explorer

# lovely hack to find out what debian packages our executable needs
DEPENDS=$(dpkg --search `readelf -d ./usr/share/foundationdb-explorer/foundationdb-explorer | egrep --only-matching '[^[]+\.so\.[^]]'` \
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
