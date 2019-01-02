#!/bin/bash 
set -eu -o pipefail

cd `dirname $0`

for file in $(ls src | grep -P ".m[l|li]$"); do
  ocamlformat -i "src/${file}"
done
