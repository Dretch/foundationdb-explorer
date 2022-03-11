#!/bin/bash

set -ex

DIR=.make-deb
VERSION=`git describe --tags --exact-match || git rev-parse HEAD`
VERSION=${VERSION#"v"} # remove any "v" prefix

rm -rf "$DIR"
mkdir -p "$DIR/usr/bin" "$DIR/usr/share/applications" "$DIR/usr/share/pixmaps" "$DIR/usr/share/foundationdb-explorer"
cd "$DIR"
cp ../.stack-work/install/*/*/*/bin/foundationdb-explorer ./usr/share/foundationdb-explorer/
cp ../foundationdb-explorer.desktop ./usr/share/applications
cp ../assets/icon.png ./usr/share/pixmaps/foundationdb-explorer.png
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
  --version "$VERSION" \
  --name foundationdb-explorer \
  --description 'Basic FoundationDB database browser' \
  --vendor '' \
  --maintainer '' \
  --url "https://github.com/Dretch/foundationdb-haskell" \
  $DEPENDS \
  .
