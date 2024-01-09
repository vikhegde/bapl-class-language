local lpeg = require "lpeg"
local pt = require "pt"

local function nodeNum(num) 
	return { tag = "number", val = tonumber(num)}
end
local function nodeVar(var) 
	print(pt.pt(var))
	return { tag = "var", var = var}
end

local space = lpeg.S(" \t\n")^0

local alpha = lpeg.R("AZ", "az")
local num = lpeg.R("09")
local id_special = lpeg.S("_$")
local ID = (alpha + id_special) * (alpha + id_special + num)^0
local var = ID / nodeVar * space

local decimal_digits = lpeg.R("09")^1
local hex_digits = lpeg.R("09", "af", "AF")^1
local float_digits = (lpeg.R("09")^1 * "." * lpeg.R("09")^0) + (lpeg.R("09")^0 * ".." * lpeg.R("09")^1) 
local scientific_notation = (float_digits + decimal_digits) * (lpeg.S("eE") * decimal_digits)^0
local hex_prefix = lpeg.P("0x") + lpeg.P("0X")  
local hex_number = (hex_prefix * hex_digits) 
local dec_number = decimal_digits 
local number = (hex_number + scientific_notation) / nodeNum  * space

local OP = lpeg.P("(") * space
local CP = lpeg.P(")") * space
local unaryNeg = lpeg.C(lpeg.P("-")) 
local opA = lpeg.C(lpeg.S("+-")) * space
local opM = lpeg.C(lpeg.S("*/%")) * space
local opExp = lpeg.C(lpeg.S("^")) * space
local opRel = lpeg.C(lpeg.P("<=") + lpeg.P(">=") + lpeg.P("==") + lpeg.P("!=") + lpeg.P("<") + lpeg.P(">"))* space

local function foldBin(tbl)
	print("binary", pt.pt(tbl))
	tree = tbl[1]
	for i = 2,#tbl,2 do
		tree = {tag = "binop", e1 = tree, op = tbl[i], e2 = tbl[i+1]}
	end
	return tree
end


local function foldUnary(tbl)
	print("unary", pt.pt(tbl))
	if #tbl == 1 then
		tree = {tag = "binop", e1 = {tag = "number", val = 0}, op = "+", e2 = tbl[1]}
	elseif tbl[1] == "-" then
		tree = {tag = "binop", e1 = {tag = "number", val = 0}, op = "-", e2 = tbl[2]}
	else
		error("Invalid unop")
	end
	return tree
end

local factor = lpeg.V"factor"
local unary = lpeg.V"unary"
local expTerm = lpeg.V"expTerm"
local mulTerm = lpeg.V"mulTerm"
local addTerm = lpeg.V"addTerm"
local relTerm = lpeg.V"relTerm"

local grammar = lpeg.P{"relTerm",
  factor = number + (OP * relTerm * CP) + var,
  unary = (lpeg.Ct(unaryNeg * factor + factor) / foldUnary),
  expTerm = (lpeg.Ct(unary * (opExp * unary)^0) / foldBin), 
  mulTerm = (lpeg.Ct(expTerm * (opM * expTerm)^0) / foldBin),
  addTerm = (lpeg.Ct(mulTerm * (opA * mulTerm)^0) / foldBin),
  relTerm = (lpeg.Ct(addTerm * (opRel * addTerm)^0) / foldBin)
}

grammar = space * grammar * space * -1

local function parse(input)
	return grammar:match(input)
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
	     ["^"] = "exp",
             ["<"] = "lt", ["<="] = "le", [">"] = "gt", [">="] = "ge",
     	     ["=="] = "eq", ["!="] = "neq"
     }
local function codeExp(state, ast)
	if ast.tag == "number" then
		addCode(state, "push")
		addCode(state, ast.val)
	elseif ast.tag == "var" then
		addCode(state, "load")
		addCode(state, ast.var)
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

local function run(code, mem, stack)
	pc = 1
	top = 0
	while pc <= #code do
		if code[pc] == "push" then
			top = top + 1
			stack[top] = code[pc+1]
			print("pc=", pc, code[pc], stack[top])
			pc = pc + 2
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
		elseif code[pc] == "le" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = 0
			if stack[top-1] <= stack[top] then
				accum = 1
			end
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "ge" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = 0
			if stack[top-1] >= stack[top] then
				accum = 1
			end
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "gt" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = 0
			if stack[top-1] > stack[top] then
				accum = 1
			end
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "lt" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = 0
			if stack[top-1] < stack[top] then
				accum = 1
			end
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "eq" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = 0
			if stack[top-1] == stack[top] then
				accum = 1
			end
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "neq" then
			print("pc=", pc, code[pc], stack[top-1], stack[top])
			local accum = 0
			if stack[top-1] ~= stack[top] then
				accum = 1
			end
			stack[top - 1] = accum
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "load" then
			top = top + 1
			stack[top] = mem[code[pc+1]] 
			print("pc=", pc, code[pc], stack[top])
			pc = pc + 2
		else
			error("Invalid opcode")
		end
	end
	return stack[top]
end
print(run(code, {["$k0"] = 0, ["$k1"] = 1, ["$k10"] = 10}, {}))

