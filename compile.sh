#!/bin/bash

mzscheme -m -f compiler.scm -e "(begin (compile-program $1) (exit))" > output.s
gcc driver.c output.s -o driver && ./driver
