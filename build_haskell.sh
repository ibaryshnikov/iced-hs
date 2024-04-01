#!/bin/bash

ghc -Wall -i. -odir build -hidir build libiced_hs.a main.hs
