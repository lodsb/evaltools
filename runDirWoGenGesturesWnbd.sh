#!/bin/bash
j=0; for i in $(ls logs/*.slog); do echo $i; Rscript plot2.R "../$i/" "../$i/plot.pdf" ; done

