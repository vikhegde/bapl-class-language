local lpeg = require "lpeg"
local pt = require "pt"

-- Last exercise of week before (Week-6) just carried over (for continuity)

local function node(tag, ...)
	local labels = table.pack(...)  -- create table (list) with varargs
	local params = table.concat(labels, ", ")
	local fields = string.gsub(params, "%w+", "%1 = %1")
	local code = string.format("return function (%s) return { tag = '%s', %s} end",
		params, tag, fields)
	return load(code)()
end

--[[ local function node(tag, ...)
	local labels = table.pack(...)
	return function(...)
			local params = table.pack(...)
			nodeTable = {tag = tag}
			for i = 1,#labels do
			        label = labels[i]
				nodeTable[label] = params[i]
			end
			return nodeTable
		end
end
--]]

local function nodeStat(lnode, exp) 
	return node("assign", "lhs", "exp")(lnode, exp)
end

local function nodeSeq(stat, stats) 
	if stats == nil then
		return stat
	else
		return node("seq", "stat", "stats")(stat, stats)
	end
end

local space = lpeg.V"space"
local function T(token)
	return lpeg.P(token) * space
end
local alphanumuscore = lpeg.R("az", "AZ", "09", "__")^1

reserved_words = {"return", "if", "else", "elseif", "while", "new"}
exclusion_list = lpeg.P(false)
for i = 1,#reserved_words do
	exclusion_list = exclusion_list + reserved_words[i]
end

local function RESV(token)
	assert(exclusion_list:match(token))
	return lpeg.P(token) * lpeg.P(function (s, p) if alphanumuscore:match(string.sub(s,p,p)) then return false else return true end end) * space
end
local maxline = 1
local maxpos = 0
local start_id = -1
local stop_id = -1
local incomment = false
local alpha = lpeg.R("AZ", "az")
local num = lpeg.R("09")
local id_special = lpeg.S("_$")
local ID = (alpha + id_special) * lpeg.P(function (s,p) start_id = p-1; return true end) * (alpha + id_special + num)^0 * lpeg.P(function (s, p) stop_id = p-1; string.sub(s, start_id, stop_id); if exclusion_list:match(string.sub(s, start_id, stop_id)) then print(string.sub(s, start_id, stop_id)); return false else return true end end)


local decimal_digits = lpeg.R("09")^1
local hex_digits = lpeg.R("09", "af", "AF")^1
local float_digits = (lpeg.R("09")^1 * "." * lpeg.R("09")^0) + (lpeg.R("09")^0 * ".." * lpeg.R("09")^1) 
local scientific_notation = (float_digits + decimal_digits) * (lpeg.S("eE") * decimal_digits)^0
local hex_prefix = lpeg.P("0x") + lpeg.P("0X")  
local hex_number = (hex_prefix * hex_digits) 
local dec_number = decimal_digits 
local number = (hex_number + scientific_notation) / tonumber / node("number", "val")  * space

local opA = lpeg.C(lpeg.S("+-")) * space
local opM = lpeg.C(lpeg.S("*/%")) * space
local opExp = lpeg.C(lpeg.S("^")) * space
local opRel = lpeg.C(lpeg.P("<=") + lpeg.P(">=") + lpeg.P("==") + lpeg.P("!=") + lpeg.P("<") + lpeg.P(">"))* space
local opBinLog = lpeg.C(lpeg.P("and") + lpeg.P("or")) * space
local ret = RESV"return" * space

local function foldBin(tbl)
	tree = tbl[1]
	for i = 2,#tbl,2 do
		tree = {tag = "binop", e1 = tree, op = tbl[i], e2 = tbl[i+1]}
	end
	return tree
end

local function foldUnary(tbl)
	print(pt.pt(tbl))
	if #tbl == 1 then
		tree = {tag = "binop", e1 = {tag = "number", val = 0}, op = "+", e2 = tbl[1]}
	elseif tbl[1] == "-" then
		tree = {tag = "binop", e1 = {tag = "number", val = 0}, op = "-", e2 = tbl[2]}
	elseif tbl[1] == "!" then
		tree = {tag = "binop", e1 = {tag = "number", val = 0}, op = "==", e2 = tbl[2]}
	else
		error("Invalid unop")
	end
	return tree
end

local function foldIndex(lst)
	print(pt.pt(lst))
	tree = lst[1]
	for i = 2,#(lst)-1 do
		tree = {tag = "getindex", array = tree, index = lst[i]}
	end
	return tree, lst[#lst]
end

local function foldNewIndex(lst)
	print(pt.pt(lst))
	tree = {tag = "newarray", lst = lst}
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
local elseifstat = lpeg.V"elseifstat"
local whilestat = lpeg.V"whilestat"
local relTerm = lpeg.V"relTerm"
local rvalue = lpeg.V"rvalue"
local lvalue = lpeg.V"lvalue"
local newExp = lpeg.V"newExp"

local grammar = lpeg.P{"prog",
  prog = space * stats * space * -1,
  stats = (stat * (T";" * stats)^0 / nodeSeq * T";"^-1) + (block * stats^0 / nodeSeq) + (T";" * (stats / nodeSeq) ^ 0),
  block = T"{" * stats * T"}" + (T"{" * T"}"),
  elseifstat = ((RESV"elseif" + RESV"if") * exp * block * (elseifstat)^-1 * (RESV"else" * block)^-1 / node("if2", "ifcond", "thenBlock", "elseBlock")),
  whilestat = (RESV"while" * exp * block / node("while2", "whileCond", "whileBlock")),
  rvalue = (lpeg.Ct(lpeg.C(ID)/node("rvar", "rvar") * (T"[" * exp * T"]")^1) / foldIndex / node("getindex", "array", "index")) + (ID / node("rvar", "rvar")) * space, 
  lvalue = (lpeg.Ct(lpeg.C(ID)/node("lvar", "lvar") * (T"[" * exp * T"]")^1) / foldIndex / node("setindex", "array", "index")) + (ID / node("lvar", "lvar")) * space,
  newExp = lpeg.Ct(RESV"new" * space * (T"[" * exp * T"]")^1) / foldNewIndex * space,
  stat = (lvalue * T"=" * (newExp + exp)) / nodeStat + (ret * exp / node("return", "exp")) + (T"@" * exp / node("print", "exp")) + elseifstat + whilestat,
  factor = number + (T"(" * exp * T")") + rvalue,
  unary = (lpeg.Ct(lpeg.C(lpeg.S"-!") * factor + factor) / foldUnary),
  expTerm = (lpeg.Ct(unary * (opExp * unary)^0) / foldBin), 
  mulTerm = (lpeg.Ct(expTerm * (opM * expTerm)^0) / foldBin),
  addTerm = (lpeg.Ct(mulTerm * (opA * mulTerm)^0) / foldBin),
  relTerm = (lpeg.Ct(addTerm * (opRel * addTerm)^0) / foldBin),
  exp = (lpeg.Ct(relTerm * (opBinLog * relTerm)^0) / foldBin),
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


Compiler = { code = {}, vars = {}, nvars = 0}

function Compiler:addCode(op)
	code = self.code
	code[#code + 1] = op
end

local ops = {["+"] = "add", ["-"] = "sub",
	     ["*"] = "mul", ["/"] = "div", ["%"] = "mod",
	     ["^"] = "exp",
             ["<"] = "lt", ["<="] = "le", [">"] = "gt", [">="] = "ge",
     	     ["=="] = "eq", ["!="] = "neq",
     }

function Compiler:var2num(var)
	if not self.vars[var] then
		self.nvars = self.nvars + 1
		self.vars[var] = self.nvars
	end
	return self.vars[var]
end


function Compiler:var2numExists(var)
	if not self.vars[var] then
		print("Variable ", var, " is undefined") 
		error("Compilation failed")
	end
	return self.vars[var]
end

function Compiler:codeExp(ast)
	if ast.tag == "number" then
		self:addCode("push")
		self:addCode(ast.val)
	elseif ast.tag == "rvar" then
		self:addCode("load")
		self:addCode(self:var2numExists(ast.rvar))
	elseif ast.tag == "lvar" then
		self:addCode(self:var2num(ast.lvar))
	elseif ast.tag == "binop" then
		if ast.op == "and" then
			self:codeExp(ast.e1)
                	local jmp = self:codeJmpZRelP()
			self:codeExp(ast.e2)
			self:fixRelJmpHere(jmp)
        	elseif ast.op == "or" then
			self:codeExp(ast.e1)
                	local jmp = self:codeJmpNZRelP()
			self:codeExp(ast.e2)
			self:fixRelJmpHere(jmp)
		else
			self:codeExp(ast.e1)
			self:codeExp(ast.e2)
			self:addCode(ops[ast.op])
		end
	elseif ast.tag == "setindex" then
		print(pt.pt(ast))
		self:codeExp(ast.index)
		if ast.array.tag == "lvar" then
			self:addCode("load")
		end
		self:codeExp(ast.array)
		self:addCode("setindex")
	elseif ast.tag == "getindex" then
		self:codeExp(ast.index)
		if ast.array.tag == "lvar" then
			self:addCode("load")
		end
		self:codeExp(ast.array)
		self:addCode("getindex")
	elseif ast.tag == "newarray" then
		for i = 1,#(ast.lst) do
			self:codeExp(ast.lst[i])
		end
		self:addCode("push")
		self:addCode(#ast.lst)
		self:addCode("newarray")
	else
		print(ast.tag)
		error("invalid ast")
	end
end

function Compiler:currentPosition()
	return #self.code
end

function Compiler:codeJmpAbsZFor()
	self:addCode("JmpAbsZFor")
	self:addCode(0)
	return self:currentPosition()
end


function Compiler:codeJmpZRelFor()
	self:addCode("JmpRelZFor")
	self:addCode(0)
	return self:currentPosition()
end

function Compiler:codeJmpRelFor()
	self:addCode("JmpRelFor")
	self:addCode(0)
	return self:currentPosition()
end

function Compiler:codeJmpRelBack(label1)
	self:addCode("JmpRelBack")
	self:addCode(self:currentPosition() - label1 + 1)
end

function Compiler:codeJmpZRelP()
	self:addCode("JmpRelZPFor")
	self:addCode(0)
	return self:currentPosition()
end

function Compiler:codeJmpNZRelP()
	self:addCode("JmpRelNZPFor")
	self:addCode(0)
	return self:currentPosition()
end

function Compiler:fixRelJmpHere(jmp)
	code[jmp] = self:currentPosition() - jmp
end

function Compiler:fixAbsJmpHere(jmp)
	code[jmp] = jmp
end
function Compiler:codeStat(ast)
	if ast.tag == "assign" then
		if ast.lhs.tag == "lvar" then
			self:codeExp(ast.exp)
			self:addCode("store")
			self:addCode(self:var2num(ast.lhs.lvar))
		elseif ast.lhs.tag == "setindex" then
			self:codeExp(ast.exp)
			self:codeExp(ast.lhs)
		else error("invalid lvalue")
		end
	elseif ast.tag == "seq" then
		self:codeStat(ast.stat)
		self:codeStat(ast.stats)
	elseif ast.tag == "return" then
		self:codeExp(ast.exp)
		self:addCode("ret")
	elseif ast.tag == "print" then
		self:codeExp(ast.exp)
		self:addCode("prt")
	elseif ast.tag == "if2" then
		self:codeExp(ast.ifcond)
		local jmp = self:codeJmpRelZFor()
		self:codeStat(ast.thenBlock)
		if ast.elseBlock == nil then
			self:fixRelJmpHere(jmp)
		else
			local jmp2 = self:codeJmpRelFor()
			self:fixRelJmpHere(jmp)
			self:codeStat(ast.elseBlock)
			self:fixRelJmpHere(jmp2)
		end
	elseif ast.tag == "while2" then
		local label1 = self:currentPosition()
		self:codeExp(ast.whileCond)
		local jmp = self:codeJmpZRelFor()
		if ast.whileBlock then
			self:codeStat(ast.whileBlock)
		end
		self:codeJmpRelBack(label1)
		self:fixRelJmpHere(jmp)
	else
		print(ast.tag)
		error("invalid ast")
	end
end

local function compile(compiler, ast)
	compiler:codeStat(ast)
	compiler:addCode("push")
	compiler:addCode(0)
	compiler:addCode("ret")
	return compiler.code
end
local code = compile(Compiler, ast)
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
			local accum = 0
			-- add may sometimes get string values. add then becomes the identity operator provided other operand is 0
			if stack[top-1] == 0 and type(stack[top]) == "string" then
				accum = stack[top]
			elseif type(stack[top-1]) == "string" or type(stack[top]) == "string" then
				error("Runtime error: attempt to add "..type(stack[top-1]).." and "..type(stack[top]))
			else
				accum = stack[top-1] + stack[top]
			end
			print(accum)
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
			print("pc=", pc, code[pc], mem[code[pc+1]])
			top = top + 1
			stack[top] = mem[code[pc+1]] 
			pc = pc + 2
		elseif code[pc] == "store" then
			mem[code[pc+1]] = stack[top]
			print("pc=", pc, code[pc], code[pc+1], stack[top])
			pc = pc + 2
			top = top - 1
		elseif code[pc] == "prt" then
			print("pc=", pc, code[pc], stack[top])
			if type(stack[top]) == "string" then
				print(pt.pt(stack[top]))
			else
				print(stack[top])
			end
			pc = pc + 1
			top = top - 1
		elseif code[pc] == "JmpAbsZ" then
			print("pc=", pc, code[pc], stack[top])
			pc = pc + 1
			if stack[top] == 0 or stack[top] == nil then
				pc = code[pc]
		        else
				pc = pc + 1
			end
			top = top - 1
		elseif code[pc] == "JmpRelZFor" then
			print("pc=", pc, code[pc], code[pc+1])
			pc = pc + 1
			if stack[top] == 0 or stack[top] == nil then
				pc = pc + 1 + code[pc]
		        else
				pc = pc + 1
			end
			top = top - 1
		elseif code[pc] == "JmpRelFor" then
			print("pc=", pc, code[pc], code[pc+1])
			pc = pc + 1
			pc = pc + 1 + code[pc]
		elseif code[pc] == "JmpRelBack" then
			print("pc=", pc, code[pc], code[pc+1])
			pc = pc + 1
			pc = pc + 1 - code[pc]
		elseif code[pc] == "JmpRelZPFor" then
			print("pc=", pc, code[pc], code[pc+1])
			pc = pc + 1
			if stack[top] == 0 then
				pc = pc + 1 + code[pc]
			else
				top = top - 1
				pc = pc + 1
			end
		elseif code[pc] == "JmpRelNZPFor" then
			print("pc=", pc, code[pc], code[pc+1])
			pc = pc + 1
			if stack[top] ~= 0 then
				pc = pc + 1 + code[pc]
			else
				top = top - 1
				pc = pc + 1
			end
		elseif code[pc] == "getindex" then
			print("pc=", pc, code[pc], stack[top], stack[top-1])
			if stack[top-1] <= 0 or stack[top-1] > stack[top]["size"] then
				error("Runtime error: array access beyond bounds, array name = "..pt.pt(stack[top])) 
				os.exit()
			end
			stack[top-1] = stack[top][stack[top-1]]
			top = top - 1
			pc = pc + 1 
		elseif code[pc] == "setindex" then
			print("pc=", pc, code[pc], stack[top], stack[top - 1], stack[top-2])
                        print(pt.pt((stack[top])))
			if stack[top-1] <= 0 then
				error("Runtime error: array access beyond bounds, array name = ", mem[code[pc+1]]) 
				os.exit()
                        elseif stack[top-1] > stack[top]["size"] then
                        	stack[top]["size"] = stack[top-1]
			end
			stack[top][stack[top-1]] = stack[top-2]
			pc = pc + 1 
			top = top - 3 
		elseif code[pc] == "newarray" then
			print("pc=", pc, code[pc], stack[top])
			index_count = stack[top]
			top = top - 1
			local array = nil
			local stack2 = {}
			local top2 = 1
			local start2 = 1
			stack2[1] = {size = stack[top]}
			top = top - 1
			for i = 2,index_count do
				incr = 0
				for j = start2,top2 do
					for k = 1,stack2[j]["size"] do
						stack2[j][k] = {size = stack[top]}
                                                stack2[top2+j*k] = stack2[j][k] 
					end
					incr = incr + stack2[j]["size"]
				end
				start2 = top2+1
				top2 = top2 + incr
				top = top -1 
			end
			stack[top] = stack2[1]
			pc = pc + 1
		else
			print(code[pc])
			error("Invalid opcode")
		end
	end
	return stack[top]
end
mem = {["$k0"] = 0, ["$k1"] = 1, ["$k10"] = 10}
stack = {}
run(code, mem, stack)
print(stack[1])

