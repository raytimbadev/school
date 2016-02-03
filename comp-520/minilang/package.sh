#!/bin/bash

set -e

cd src
cabal build
cd ../..
rm -rf jacob_errington
cp -rf minilang jacob_errington
mv -v jacob_errington/{src/dist/build/minilangc/minilangc,}
rm -rf jacob_errington/src/{.cabal-sandbox,cabal.sandbox.config}
rm -rf jacob_errington/src/dist
tar zcvf jacob_errington.tar.gz jacob_errington/
