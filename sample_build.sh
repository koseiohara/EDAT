#!/bin/bash

./configure --prefix=$HOME/FortranLib FC="ifort" CC="icc" FCFLAGS="-O3 -traceback -warn all -convert little_endian -assume byterecl" CFLAGS="-O3 -Wall"

