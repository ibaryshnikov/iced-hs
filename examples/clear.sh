#!/bin/bash

set -e

for folder in */ ; do
  if [ -f ${folder}main ]; then
    echo "Clearing $folder" | sed 's/\/$//'
    rm ${folder}main
    echo -e "Done\n"
  fi
done
