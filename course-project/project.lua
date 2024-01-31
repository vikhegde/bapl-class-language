-- Grammar for the pug language.
local lpeg = require"lpeg"
local pt = require"pt"

-- Parsing for filler text like whitespace and comments - has no capture 
local whitespace = lpeg.S" \n\t"
local blockcmnt = lpeg.P("#{") * ((lpeg.P(1) - lpeg.P("#}"))^0) * lpeg.P("#}") 
local linecmnt = lpeg.P("#") * ((lpeg.P(1) - lpeg.P("\n"))^0) * lpeg.P("\n") 

-- Numbers - integers and floating point

-- Order is important. Float comes before integer
-- Among floats exponential notation comes before regular floats
-- Among integers, Hex and binary come before octal which comes before decimal (lpeg is possesive)
local decimalPointPrefix = (lpeg.R("09")^1) * lpeg.P(".") * (lpeg.R("09")^0) 
local decimalPointSuffix = (lpeg.R("09")^0) * lpeg.P(".") * (lpeg.R("09")^1)
local nonExponentFloat = decimalPointPrefix + decimalPointSuffix
local exponentialFloat = nonExponentFloat * lpeg.S("eE") * (lpeg.R("09")^1)
local float = exponentialFloat + nonExponentFloat
local hexInteger = (lpeg.P("0x") + lpeg.P("0X")) * ((lpeg.R("09", "af", "AF"))^1)
local binaryInteger = (lpeg.P("0b") + lpeg.P("0B")) * ((lpeg.R("01"))^1)
local octInteger = lpeg.P("0") * ((lpeg.R("07"))^1)
local decimalInteger = (lpeg.R("09")^1)
local integer = hexInteger + binaryInteger + octInteger + decimalInteger
local number = lpeg.C(float + integer) / tonumber

-- Parsing for additive operators
local opAdd = lpeg.S"+-"

-- Parsing for multiplicative operators
local opMul = lpeg.S"*/%"

-- Parsing for exponential operator
local opExpo = lpeg.P"^"

-- Parsing for unary minus
local opUnaryMinus = lpeg.P("-")


local nop = lpeg.V"nop"

local grammar = lpeg.P{"prog",
			prog = (nop ^ 0) * (number ^ 1) * (nop ^ 0),
                        nop = blockcmnt + linecmnt + whitespace  -- order is important lpeg is possesive 
		}

Compiler = {grammar = {}, ast = {}, code = {}}
Compiler.grammar = grammar * -1


function Compiler:parse(text)
	self.ast = self.grammar:match(text)
	if not self.ast then
		print("Syntax Error: Failed to parse pug program")
	end
end
local input = io.read("*a")
Compiler:parse(input)
print(pt.pt(Compiler.ast))
