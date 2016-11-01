#!/usr/bin/env bash

stack build && \
echo "running ----" && \
stack exec haskell-diagrams-cellular-automata-exe  --   -h 32 -w 1024 -o images/simple.gif
