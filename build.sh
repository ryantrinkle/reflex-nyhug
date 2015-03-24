#!/bin/sh

nix-shell --command "ghcjs --make -isrc src/Main.hs -o static/ -O2"
echo "Done at `date`"
