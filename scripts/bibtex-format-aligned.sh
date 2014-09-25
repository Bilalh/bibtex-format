#!/bin/bash
set -o nounset
bibtex-format "$@" | align_equals.rb
