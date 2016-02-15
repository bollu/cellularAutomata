#!/usr/bin/env bash
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" \
echo "profiling ----" && \
stack exec haskell-diagrams-cellular-automata-exe  --   -h 400 -w 400 -o images/simple.gif +RTS -p
