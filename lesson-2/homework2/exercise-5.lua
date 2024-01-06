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
local opM = lpeg.C(lpeg.S("*/%")) * space
local opExp = lpeg.C(lpeg.S("^")) * space

local function foldBin(tbl)
	tree = tbl[1]
	for i = 2,#tbl,2 do
		tree = {tag = "binop", e1 = tree, op = tbl[i], e2 = tbl[i+1]}
	end
	return tree
end

local expTerm = (lpeg.Ct(number * (opExp * number)^0) / foldBin) 
local mulTerm = (lpeg.Ct(expTerm * (opM * expTerm)^0) / foldBin)
local addTerm = space * (lpeg.Ct(mulTerm * (opA * mulTerm)^0) / foldBin) * space * -1

print(pt.pt(addTerm:match("0xff")))
print(pt.pt(addTerm:match("0xFF")))
print(pt.pt(addTerm:match("0XFF")))
print(pt.pt(addTerm:match("0Xff")))
print(pt.pt(addTerm:match("ff")))
print(pt.pt(addTerm:match("45")))

local function parse(input)
	return addTerm:match(input)
end

local input = io.read("a")
local ast = parse(input)
print(pt.pt(ast))

local function addCode(state, op)
	code = state.code
	code[#code + 1] = op
end

local ops = {["+"] = "add", ["-"] = "sub",
	     ["*"] = "mul", ["/"] = "div", ["%"] = "mod",
	     ["^"] = "exp"}
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
			print("pc=", pc, code[pc], stack[top])
		elseif code[pc] == "add" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = stack[top-1] + stack[top]
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "sub" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = stack[top-1] - stack[top]
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "mul" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = stack[top-1] * stack[top]
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "div" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = stack[top-1] / stack[top]
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "mod" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = stack[top-1] % stack[top]
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "exp" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = stack[top-1] ^ stack[top]
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

