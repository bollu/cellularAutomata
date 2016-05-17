#!/usr/bin/env bash

stack build && \
echo "running ----" && \
stack exec haskell-diagrams-cellular-automata-exe  --   -h 512 -w 512 -o images/simple.gif
