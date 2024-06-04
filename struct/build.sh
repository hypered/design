#! /usr/bin/env bash

nix-shell ../../slab/default.nix -A shell --run 'slab build templates/'
