#!/usr/bin/env ruby -w
# 
# Runs a bunch of test cases, wee!

require 'test/unit'

class TestCompiler < Test::Unit::TestCase

	def get_return(input)
	`./compile.sh "#{input}"`.chomp
	end

	def test_integers
		tests = ["-536870912",
					"536870911",
					"0",
					"123",
					"-32145",
					"12345678",
					"7",
					"41" ]
		tests.each do |test_case|
			assert_equal(test_case, get_return(test_case))
		end
	end

	def test_immediate
		tests = {
			"null"		=> 	"()",
			"()"			=>		"()",
			"#\\Space"	=>		" ",
			"#\\A"		=>		"A",
			"#\\a"		=>		"a",
			"#\\_"		=>		"_",
			"#t"			=>		"#t",
			"#f"			=>		"#f"
		}

		tests.each_pair do |test_case, result|
			assert_equal(result, get_return(test_case))
		end
	end

	def test_unary
		tests = {
			"'(add1 4)"			=>		"5",
			"'(add1 536870910)"=>		"536870911",
			# this test overflows
			"'(add1 536870911)"=>		"-536870912",
			"'(add1 (add1 (add1 (add1 (add1 6)))))"	=>	"11",
			"'(sub1 4)"			=>		"3",
			"'(sub1 -536870912)"=>		"536870911",
			# this test overflows
			"'(sub1 536870911)"=>		"536870910",
			"'(sub1 (sub1 (sub1 (sub1 (sub1 6)))))"	=>	"1",
			"'(char->integer (integer->char 13))"		=> "13",
			"'(char->integer (integer->char 41))"		=> "41",
			"'(char->integer (integer->char 1000))"	=> "1000",
			"'(integer->char (char->integer #\\Space))" => " ",
			"'(integer->char (char->integer #\\!))" => "!",
			"'(integer->char (char->integer #\\a))" => "a",
			"'(integer->char (char->integer #\\Z))" => "Z",
			"'(zero? 0)"		=>		"#t",
			"'(zero? 536870911)"		=>		"#f",
			"'(zero? -0)"		=>		"#t",
			"'(zero? -536870912)"		=>		"#f",
			"'(null? null)"		=>		"#t",
			"'(null? ())"		=>		"#t",
			"'(null? #\\c)"		=>		"#f",
			"'(null? (add1 (sub1 4)))"		=>		"#f",
			"'(not #f)"			=>			"#t",
			"'(not #t)"			=>			"#f",
			"'(not 3)"			=>			"#f",
			"'(not 0)"			=>			"#f",
			"'(not null)"			=>			"#f",
			"'(not ())"			=>			"#f",
			"'(not (null? null))"			=>			"#f",
			"'(not (null? 6))"			=>			"#t",
			"'(integer? 100)"			=>			"#t",
			"'(integer? null)"			=>			"#f",
			"'(integer? (add1 4))"			=>			"#t",
			"'(integer? #\\c)"			=>			"#f",
			"'(integer? -3213)"			=>			"#t",
			"'(integer? ())"			=>			"#f",
			"'(integer? -0)"			=>			"#t",
			"'(boolean? #t)"			=>			"#t",
			"'(boolean? #f)"			=>			"#t",
			"'(boolean? (integer? ()))"			=>			"#t",
			"'(boolean? (integer? 3))"			=>			"#t",
			"'(boolean? 3)"			=>			"#f",
			"'(boolean? null)"			=>			"#f"
		}

		tests.each_pair do |test_case, result|
			assert_equal(result, get_return(test_case))
		end
	end

	def test_binary
	end
end
