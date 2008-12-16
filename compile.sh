#!/bin/bash

if [ "$1" = "--debug" ]; then
	if [ -e "$2" ]; then
		mzscheme  -f compiler.scm -e "(begin (define debug #t) (compile-program '`cat $2`) (exit))" > output.s
	else
		mzscheme  -f compiler.scm -e "(begin (define debug #t) (compile-program $2) (exit))" > output.s
	fi

	cat output.s
elif [ "$1" = "--test" ]; then
	mzscheme -f compiler.scm -e "(begin (define debug #f) (test-all) (exit))"
elif [ "$1" = "--indiv-test" ]; then
	mzscheme -f compiler.scm -e "(begin (define debug #f) (compile-program $2) (exit))" > stst.s

	# if the file contained no functions, add the main heading
	if [ "`grep 'scheme_entry' stst.s`" == "" ]; then
		echo ".text" >> newoutput
		echo ".globl _scheme_entry" >> newoutput
		echo "_scheme_entry:" >> newoutput
		cat stst.s >> newoutput
		mv newoutput stst.s
	fi
	#gcc driver.c stst.s -o driver && ./driver
else
	if [ -e "$1" ]; then
		mzscheme  -f compiler.scm -e "(begin (define debug #f) (compile-program '`cat $1`) (exit))" > output.s
	else
		mzscheme  -f compiler.scm -e "(begin (define debug #f) (compile-program $1) (exit))" > output.s
	fi

	# if the file contained no functions, add the main heading
	if [ "`grep 'scheme_entry' output.s`" == "" ]; then
		echo ".text" >> newoutput
		echo ".globl _scheme_entry" >> newoutput
		echo "_scheme_entry:" >> newoutput
		cat output.s >> newoutput
		mv newoutput output.s
		gcc driver.c output.s -o driver && ./driver
	else
		echo "Error: "
		cat output.s
	fi
fi
