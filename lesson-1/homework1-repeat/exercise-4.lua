local lpeg = require "lpeg"

local number = lpeg.C(lpeg.R("09")^1) 
local optspace = lpeg.P(" ")^0
local plus_space = optspace * lpeg.Cp() * lpeg.P("+") * optspace
local pattern = optspace * number * optspace * (plus_space * optspace * number * optspace) ^ 0 * -1

print(pattern:match("4"))
print(pattern:match(" 4"))
print(pattern:match("4 "))
print(pattern:match(" 4 "))
print(pattern:match("4+2"))
print(pattern:match("4+ 2"))
print(pattern:match("4 +2"))
print(pattern:match("4 + 2"))
print(pattern:match("4 + 2 "))
print(pattern:match("4 + 2 +"))
print(pattern:match("12+13+25"))
print(pattern:match("12+13+25 -"))
print(pattern:match("4+"))
