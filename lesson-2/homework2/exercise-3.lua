local lpeg = require "lpeg"
local pt = require "pt"

local function node(num) 
	return { tag = "number", val = tonumber(num)}
end
local decimal_digits = lpeg.R("09")^1
local hex_digits = lpeg.R("09", "af", "AF")^1
local space = lpeg.S(" \t\n")^0
local hex_prefix = lpeg.P("0x") + lpeg.P("0X")  
local hex_number = (hex_prefix * hex_digits) 
local dec_number = decimal_digits 
local number = (hex_number + dec_number) / node  * space

local opA = lpeg.C(lpeg.S("+-")) * space

local function foldBin(tbl)
	tree = tbl[1]
	for i = 2,#tbl,2 do
		tree = {tag = "binop", e1 = tree, op = tbl[i], e2 = tbl[i+1]}
	end
	return tree
end

local numexp = space * (lpeg.Ct(number * (opA * number)^0) / foldBin) * space * -1

print(pt.pt(numexp:match("0xff")))
print(pt.pt(numexp:match("0xFF")))
print(pt.pt(numexp:match("0XFF")))
print(pt.pt(numexp:match("0Xff")))
print(pt.pt(numexp:match("ff")))
print(pt.pt(numexp:match("45")))

local function parse(input)
	return numexp:match(input)
end

local input = io.read("a")
local ast = parse(input)
print(pt.pt(ast))

local function addCode(state, op)
	code = state.code
	code[#code + 1] = op
end

local ops = {["+"] = "add", ["-"] = "sub"}
local function codeExp(state, ast)
	if ast.tag == "number" then
		addCode(state, "push")
		addCode(state, ast.val)
	elseif ast.tag == "binop" then
		codeExp(state, ast.e1)
		codeExp(state, ast.e2)
		addCode(state, ops[ast.op])
	else
		error("invalid ast")
	end
end
local function compile(ast)
	local state = { code = {}}
	codeExp(state, ast)
	return state.code
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
		elseif code[pc] == "add" then
			local accum = stack[top-1] + stack[top]
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "sub" then
			local accum = stack[top-1] - stack[top]
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		else
			error("Invalid opcode")
		end
	end
	return stack[top]
end
print(run(code, {}))

