#!/usr/bin/env lua

dofile("checkok.lua")

local lpeg = require "lpeg"
local capture_pattern = (lpeg.C(lpeg.R("09")^1) * lpeg.P(" ")^0 * lpeg.Cp() * "+" * lpeg.P(" ")^0 * lpeg.C(lpeg.R("09")^1)) *  ((lpeg.P(" ")^0 * lpeg.Cp() * "+" * lpeg.P(" ")^0) * lpeg.C(lpeg.R("09")^1))^0

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
