#!/bin/bash

ghc -Wall -i../.. \
  -odir ../../build \
  -hidir ../../build \
  ../../libiced_hs.a \
  -threaded \
  main.hs
