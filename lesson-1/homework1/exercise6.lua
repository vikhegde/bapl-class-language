#!/usr/bin/env lua

dofile("checkok.lua")

function fold(table)
	local acc = table[1]
	for i = 2, #table, 2 do
		if table[i] == '+' then
			acc = acc + table[i+1]
		elseif table[i] == '-' then
			acc = acc - table[i+1]
		elseif table[i] == '*' then
			acc = acc * table[i+1]
		elseif table[i] == '/' then
			acc = acc / table[i+1]
		elseif table[i] == '%' then
			acc = acc % table[i+1]
		elseif table[i] == '^' then
			acc = acc ^ table[i+1]
		else
			error("unknown operator")
		end
	end
	return acc
end

local lpeg = require "lpeg"
local optspace = lpeg.P(" ")^0
local number = lpeg.C(lpeg.R("09") ^ 1 * optspace) / tonumber
local add = lpeg.C(lpeg.S("+-")) * optspace
local mul = lpeg.C(lpeg.S("*/%")) * optspace
local exp = lpeg.C(lpeg.P("^")) * optspace
local termExp = lpeg.Ct(number * (exp * number)^0) / fold
local termMul = lpeg.Ct(termExp * (mul * termExp)^0) / fold
local math_pattern = lpeg.Ct(termMul * (add * termMul)^0) / fold * -1

print(math_pattern:match("1"))
print(math_pattern:match("2^3"))
print(math_pattern:match("5 * 2"))
print(math_pattern:match("6 - 3"))
print(math_pattern:match("7+5"))
print(math_pattern:match("20 % 6"))
print(math_pattern:match("1+"))
print(math_pattern:match("1 +"))
print(math_pattern:match("1 + "))
print(math_pattern:match("2 * 4 + 3"))
print(math_pattern:match("2*4 + 2*5^3 - 100"))
print(math_pattern:match("12+13%25"))
