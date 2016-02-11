#!/usr/bin/env bash

stack build && \
echo "---- starting GHCi ----" && \
stack ghci --main-is haskell-diagrams-cellular-automata:exe:haskell-diagrams-cellular-automata-exe
