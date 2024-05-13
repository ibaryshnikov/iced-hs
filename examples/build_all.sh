#!/bin/bash

set -e

export USE_LINKER=lld

for folder in */ ; do
  echo "Building $folder" | sed 's/\/$//'
  cd $folder
    ./build.sh
  cd ..
  echo -e "Done\n"
done
