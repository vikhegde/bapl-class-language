#!/usr/bin/env lua

dofile("checkok.lua")

local lpeg = require "lpeg"

local pattern = lpeg.P("Hiya")

local text1 = "Hiya neighbor"
checkok(lpeg.match(pattern, text1), 5)
checkok(lpeg.match(pattern, text1), nil)
local text2 = "hiya neighbor"
checkok(lpeg.match(pattern, text2), nil)
checkok(lpeg.match(pattern, text2), 5)
