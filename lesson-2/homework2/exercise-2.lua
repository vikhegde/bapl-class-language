local lpeg = require "lpeg"
local pt = require "pt"

local decimal_digits = lpeg.R("09")^1
local hex_digits = lpeg.R("09", "af", "AF")^1
local space = lpeg.S(" \t\n")^0
local hex_prefix = lpeg.P("0x") + lpeg.P("0X")  
local hex_number = (hex_prefix * hex_digits) 
local dec_number = decimal_digits 
local number = space * (hex_number + dec_number) / tonumber * -1

print(number:match("0xff"))
print(number:match("0xFF"))
print(number:match("0XFF"))
print(number:match("0Xff"))
print(number:match("ff"))
print(number:match("45"))

local function parse(num)
	return { tag = "number", num = num}
end

local input = io.read("*n")
local ast = parse(input)
print(pt.pt(ast))

local function compile(ast, code)
       	if ast.tag == "number" then
	  return {"push", ast.num}
	end
end
local code = compile(ast)
print(pt.pt(code))

local function run(code, stack)
	pc = 1
	top = 0
	while pc <= #code do
		if code[pc] == "push" then
			top = top + 1
			stack[top] = code[pc+1]
			pc = pc + 2
		end
	end
	return stack[top]
end
print(run(code, {}))
