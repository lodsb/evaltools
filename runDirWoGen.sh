#!/bin/bash
j=0; for i in $(ls logs/*.slog); do echo $i; Rscript plot.R "../$i/" "../$i/plot.pdf" ; done

