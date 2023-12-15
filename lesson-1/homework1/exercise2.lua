#!/usr/bin/env lua

dofile("checkok.lua")

local lpeg = require "lpeg"
local summation_pattern = (lpeg.R("09")^1 * lpeg.P(" ")^0 * "+" * lpeg.P(" ")^0 * lpeg.R("09")^1) *  ((lpeg.P(" ")^0 * "+" * lpeg.P(" ")^0) * lpeg.R("09")^1)^0

checkok(summation_pattern:match("1"),nil)
checkok(summation_pattern:match("1 + 2"),6)
checkok(summation_pattern:match("1+ 2"),5)
checkok(summation_pattern:match("1 +2"),5)
checkok(summation_pattern:match("1+2"),4)
checkok(summation_pattern:match("1 +"),nil)
checkok(summation_pattern:match("1+"),nil)
checkok(summation_pattern:match("+1"),nil)
checkok(summation_pattern:match("+ 1"),nil)
checkok(summation_pattern:match("1+2+3"),6)
checkok(summation_pattern:match("1 + 2 + 3"),10)
