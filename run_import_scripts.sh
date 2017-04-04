#! /bin/bash
for f in $1/*; do
  Rscript "$f"
done
