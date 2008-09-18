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
	end

	def test_binary
	end
end
