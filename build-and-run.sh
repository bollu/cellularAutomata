#!/usr/bin/env bash

stack build && \
echo "running ----" && \
stack exec haskell-diagrams-cellular-automata-exe  --   -h 256 -w 256 -o images/simple.gif
