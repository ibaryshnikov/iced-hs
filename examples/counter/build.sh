#!/bin/bash

cd ../..
ghc -Wall libiced_hs.a Iced.hs \
  examples/counter/main.hs \
  -o examples/counter/main
