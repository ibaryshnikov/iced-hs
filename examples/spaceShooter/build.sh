#!/bin/bash

if [ ! -z ${USE_LINKER+x} ]; then
  echo "Linker is $USE_LINKER"
fi

ghc -Wall -i../../src \
  -odir ../../build \
  -hidir ../../build \
  ../../libiced_hs.a \
  -threaded \
  ${USE_LINKER+-optl -fuse-ld="$USE_LINKER"} \
  -o main \
  Main.hs
