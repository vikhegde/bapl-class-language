local lpeg = require "lpeg"
local pt = require "pt"

-- dont allow reserved words to be used as identifiers ( example return = 10;)

local function nodeNum(num) 
	return { tag = "number", val = tonumber(num)}
end
local function nodeRVar(rvar) 
	print(pt.pt(rvar))
	return { tag = "rvar", rvar = rvar}
end

local function nodeLVar(lvar) 
	print(pt.pt(lvar))
	return { tag = "lvar", lvar = lvar}
end

local function nodeStat(lnode, exp) 
	print(pt.pt(id))
	print(pt.pt(exp))
	return { tag = "assign", lvar = lnode.lvar, exp = exp}
end

local function nodeRet(exp) 
	print(pt.pt(exp))
	return { tag = "return", exp = exp}
end


local function nodePrint(exp) 
	print(pt.pt(exp))
	return { tag = "print", exp = exp}
end

local function nodeSeq(stat, stats) 
	print(pt.pt(stat))
	print(pt.pt(stats))
	if stats == nil then
		return stat
	else
		return { tag = "seq", stat = stat, stats = stats}
	end
end

local space = lpeg.V"space"
local function T(token)
	return lpeg.P(token) * space
end
local alphanumuscore = lpeg.R("az", "AZ", "09", "__")^1

reserved_words = {"return", "if"}
exclusion_list = lpeg.P(false)
for i = 1,#reserved_words do
	exclusion_list = exclusion_list + reserved_words[i]
end

local function RESV(token)
	print(token)
	assert(exclusion_list:match(token))
	return lpeg.P(token) * -alphanumuscore * space
end
local maxline = 1
local maxpos = 0
local incomment = false
local alpha = lpeg.R("AZ", "az")
local num = lpeg.R("09")
local id_special = lpeg.S("_$")
local ID = (alpha + id_special) * (alpha + id_special + num)^0 - (exclusion_list * -alphanumuscore) 
local rvalue = ID / nodeRVar * space
local lvalue = ID / nodeLVar * space

local decimal_digits = lpeg.R("09")^1
local hex_digits = lpeg.R("09", "af", "AF")^1
local float_digits = (lpeg.R("09")^1 * "." * lpeg.R("09")^0) + (lpeg.R("09")^0 * ".." * lpeg.R("09")^1) 
local scientific_notation = (float_digits + decimal_digits) * (lpeg.S("eE") * decimal_digits)^0
local hex_prefix = lpeg.P("0x") + lpeg.P("0X")  
local hex_number = (hex_prefix * hex_digits) 
local dec_number = decimal_digits 
local number = (hex_number + scientific_notation) / nodeNum  * space

local unaryNeg = lpeg.C(lpeg.P("-")) 
local opA = lpeg.C(lpeg.S("+-")) * space
local opM = lpeg.C(lpeg.S("*/%")) * space
local opExp = lpeg.C(lpeg.S("^")) * space
local opRel = lpeg.C(lpeg.P("<=") + lpeg.P(">=") + lpeg.P("==") + lpeg.P("!=") + lpeg.P("<") + lpeg.P(">"))* space
local ret = RESV"return" * space

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

local block = lpeg.V"block"
local stat = lpeg.V"stat"
local stats = lpeg.V"stats"
local factor = lpeg.V"factor"
local unary = lpeg.V"unary"
local expTerm = lpeg.V"expTerm"
local mulTerm = lpeg.V"mulTerm"
local addTerm = lpeg.V"addTerm"
local exp = lpeg.V"exp"
local prog = lpeg.V"prog"

local grammar = lpeg.P{"prog",
  prog = space * stats * space * -1,
  stats = (stat * (T";" * stats)^0 / nodeSeq * T";"^-1) + (block * stats^0 / nodeSeq) + (T";" * (stats / nodeSeq) ^ 0),
  block = T"{" * stats * T"}" + (T"{" * T"}"),
  stat = (lvalue * T"=" * exp) / nodeStat + (ret * exp / nodeRet) + (T"@" * exp / nodePrint), 
  factor = number + (T"(" * exp * T")") + rvalue,
  unary = (lpeg.Ct(T"-" * factor + factor) / foldUnary),
  expTerm = (lpeg.Ct(unary * (opExp * unary)^0) / foldBin), 
  mulTerm = (lpeg.Ct(expTerm * (opM * expTerm)^0) / foldBin),
  addTerm = (lpeg.Ct(mulTerm * (opA * mulTerm)^0) / foldBin),
  exp = (lpeg.Ct(addTerm * (opRel * addTerm)^0) / foldBin),
  space = (lpeg.S(" \t\n") + (lpeg.P("#{") * (lpeg.P(1) - lpeg.P("#}"))^0 * (lpeg.P("#}") - lpeg.P("\n"))) + (lpeg.P("#") * (lpeg.P(1) - lpeg.P("\n"))^0))^0
                                * lpeg.P(function (s, p) 
					      if string.sub(s, p-1, p-1) == '#' and string.sub(s, p, p) == "{" then
						      block_comment = true
				              elseif block_conment and string.sub(s, p, p) == '#' and string.sub(s,p+1,p+1) == "}" then
						      block_comment = false
				              elseif string.sub(s, p-1, p-1) == '#' then
						      line_comment = true
				              elseif line_comment and string.sub(s, p, p) == '\n' then
						      line_comment = false
					      end
					      if not block_comment and not line_comment then
						      maxpos = p
					      end

	                                      if string.sub(s,p-1,p-1)== '\n' then 
					          maxline = maxline + 1 
					      end
					      return true
					 end) 
}

local function syntaxError(input, linenum, maxpos)
    print("Syntax error detected at line ", linenum)
    print("error (^) at: ", string.sub(input, maxpos - 10, maxpos), "^", string.sub(input, maxpos+1, maxpos + 10))
    print("Compile error.")
end
local function parse(input)
	result =  grammar:match(input)
	if not result then
		syntaxError(input, maxline, maxpos)
		os.exit()
	end
	return result
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

local function var2num(state, var)
	if not state.vars[var] then
		state.nvars = state.nvars + 1
		state.vars[var] = state.nvars
	end
	return state.vars[var]
end


local function var2numExists(state, var)
	if not state.vars[var] then
		print("Variable ", var, " is undefined") 
		error("Compilation failed")
	end
	return state.vars[var]
end

local function codeExp(state, ast)
	if ast.tag == "number" then
		addCode(state, "push")
		addCode(state, ast.val)
	elseif ast.tag == "rvar" then
		addCode(state, "load")
		addCode(state, var2numExists(state, ast.rvar))
	elseif ast.tag == "lvar" then
		addCode(state, var2num(state, ast.lvar))
	elseif ast.tag == "binop" then
		codeExp(state, ast.e1)
		codeExp(state, ast.e2)
		addCode(state, ops[ast.op])
	else
		error("invalid ast")
	end
end

local function codeStat(state, ast)
	if ast.tag == "assign" then
		codeExp(state, ast.exp)
		addCode(state, "store")
		print(ast.lvar)
		addCode(state, var2num(state, ast.lvar))
	elseif ast.tag == "seq" then
		codeStat(state, ast.stat)
		codeStat(state, ast.stats)
	elseif ast.tag == "return" then
		codeExp(state, ast.exp)
		addCode(state, "ret")
	elseif ast.tag == "print" then
		codeExp(state, ast.exp)
		addCode(state, "prt")
	else
		error("invalid ast")
	end
end

local function compile(state, ast)
	codeStat(state, ast)
	addCode(state, "push")
	addCode(state, 0)
	addCode(state, "ret")
	return state.code
end
state = { code = {}, vars = {}, nvars = 0}
local code = compile(state, ast)
print(pt.pt(code))

local function run(code, mem, stack)
	pc = 1
	top = 0
	while true do
		if code[pc] == "ret" then
			print("pc=", pc, code[pc], stack[1])
			return
		elseif code[pc] == "push" then
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
			print("pc=", pc, code[pc], mem[code[pc+1]], stack[top])
			pc = pc + 2
		elseif code[pc] == "store" then
			mem[code[pc+1]] = stack[top]
			print("pc=", pc, code[pc], code[pc+1], stack[top])
			pc = pc + 2
			top = top - 1
		elseif code[pc] == "prt" then
			print("pc=", pc, code[pc], stack[top])
			print(stack[top])
			pc = pc + 1
			top = top - 1
		else
			error("Invalid opcode")
		end
	end
	return stack[top]
end
mem = {["$k0"] = 0, ["$k1"] = 1, ["$k10"] = 10}
stack = {}
run(code, mem, stack)
print(stack[1])

