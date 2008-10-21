#!/bin/bash

if [ "$1" = "--debug" ]; then
	mzscheme -m -f compiler.scm -e "(begin (define debug #t) (compile-program $2) (exit))" > output.s
	cat output.s
else
	mzscheme -m -f compiler.scm -e "(begin (define debug #f) (compile-program $1) (exit))" > output.s
	# nothing gets outputted
	if [ "`wc -l output.s | cut -d' ' -f1`" -ne "4" ]; then
		# if the file contained no functions, add the main heading
		if [ "`grep 'scheme_entry' output.s`" == "" ]; then
			echo ".text" >> newoutput
			echo ".globl _scheme_entry" >> newoutput
			echo "_scheme_entry:" >> newoutput
			cat output.s >> newoutput
			mv newoutput output.s
		fi
		gcc driver.c output.s -o driver && ./driver
	else
		echo "Error: "
		cat output.s
	fi
fi
