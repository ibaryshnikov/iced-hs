#!/bin/bash

set -e

for folder in */ ; do
  echo "Clearing $folder" | sed 's/\/$//'
  rm ${folder}main
  echo -e "Done\n"
done
