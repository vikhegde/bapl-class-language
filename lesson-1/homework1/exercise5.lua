#!/usr/bin/env lua

dofile("checkok.lua")

function fold(table)
	local acc = table[1]
	for i = 2, #table do
		acc = acc + table[i]
	end
	return acc
end

local lpeg = require "lpeg"
local optspace = lpeg.P(" ") ^ 0
local number = lpeg.C(lpeg.S("+-")^-1 * lpeg.R("09") ^ 1 * optspace) / tonumber
local plus = "+" * optspace
local capture_pattern = lpeg.Ct(optspace * number * (plus * number)^0) / fold * -1

print(capture_pattern:match("1"))
print(capture_pattern:match("1 + 2"))
print(capture_pattern:match("1+ 2"))
print(capture_pattern:match("1 +2"))
print(capture_pattern:match("1+2"))
print(capture_pattern:match("1 +"))
print(capture_pattern:match("1+"))
print(capture_pattern:match("+1"))
print(capture_pattern:match("+ 1"))
print(capture_pattern:match("12+13+25"))
