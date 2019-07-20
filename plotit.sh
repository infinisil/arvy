#!/usr/bin/env gnuplot
set terminal png size 2000,2000 enhanced font "arial,22.0"
set output 'plot.png'
set logscale x
plot 'initiallyMst' using 1:2 with lines, 'initiallyRandom' using 1:2 with lines
