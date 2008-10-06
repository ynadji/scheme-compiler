#!/bin/bash

if [ "$1" = "--debug" ]; then
	mzscheme -m -f compiler.scm -e "(begin (define debug #t) (compile-program $2) (exit))" > output.s
	cat output.s
else
	mzscheme -m -f compiler.scm -e "(begin (define debug #f) (compile-program $1) (exit))" > output.s
	if [ "`wc -l output.s | cut -d' ' -f1`" -ne "4" ]; then
		gcc driver.c output.s -o driver && ./driver
	else
		echo "Error: "
		cat output.s
	fi
fi
