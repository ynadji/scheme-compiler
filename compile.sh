#!/bin/bash

echo 'mzscheme -m -f compiler.scm -e "(begin (compile-program $1) (exit))" > output.s'
mzscheme -m -f compiler.scm -e "(begin (compile-program $1) (exit))" > output.s
echo 'gcc driver.c output.s -o driver'
gcc driver.c output.s -o driver
