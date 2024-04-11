#!/bin/bash

set -e

for folder in */ ; do
  echo "Building $folder"
  cd $folder
    ./build.sh
  cd ..
  echo -e "Done\n"
done
