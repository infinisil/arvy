#!/usr/bin/env gnuplot
set terminal png size 2000,2000 enhanced font "arial,22.0"
set output 'plot.png'
set logscale x
plot 'ivyMst' using 1:2 with lines, 'ivyRandom' using 1:2 with lines, 'ring' using 1:2 with lines

set output 'plot-ratio.png'
unset logscale x
plot 'ratioIvy-mst' using 1:2:3 with yerrorbars, 'ratioIvy-random' using 1:2:3 with yerrorbars,
#plot 'ratioConstantRing' using 1:2:3 with yerrorbars
