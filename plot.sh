#!/bin/bash

gnuplot << EOP

datafile = "$1"

set terminal jpeg font arial 8 size 640,480
set grid x y
set key autotitle columnhead
set datafile separator ","
set output "$1.jpg"

plot datafile with lines

EOP
