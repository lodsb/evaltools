#!/bin/bash
j=0; for i in $(ls logs/*.slog); do echo $i; mkdir -p ../$i; j=$((j%3+1)); ./sbt "run $i $((j-1)) ../$i/"; Rscript plot.R "../$i/" "../$i/plot.pdf" ; done

