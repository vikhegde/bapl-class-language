local lpeg = require"lpeg"
local pt = require"pt"

-- Pattern for filler text like space and comments - has no capture 
local space = lpeg.S" \n\t"
local blockcmnt = lpeg.P("#{") * ((lpeg.P(1) - lpeg.P("#}"))^0) * lpeg.P("#}") 
local linecmnt = lpeg.P("#") * ((lpeg.P(1) - lpeg.P("\n"))^0) * lpeg.P("\n") 
local whitespace = (blockcmnt + linecmnt + space) ^ 0  -- order is important lpeg is possesive 

-- Numbers - integers and floating point

-- Order is important because of possessive property of LPEG.
-- Float comes before integer
-- Among floats exponential notation comes before regular floats
-- Among integers, Hex and binary come before octal which comes before decimal 
local decimalPointPrefix = (lpeg.R("09")^1) * lpeg.P(".") * (lpeg.R("09")^0) 
local decimalPointSuffix = (lpeg.R("09")^0) * lpeg.P(".") * (lpeg.R("09")^1)
local nonExponentFloat = decimalPointPrefix + decimalPointSuffix
local exponentialFloat = nonExponentFloat * lpeg.S("eE") * (lpeg.R("09")^1)
local float = ((exponentialFloat + nonExponentFloat) / tonumber) * whitespace
local hexInteger = (lpeg.P("0x") + lpeg.P("0X")) * ((lpeg.R("09", "af", "AF"))^1)
local binaryInteger = (lpeg.P("0b") + lpeg.P("0B")) * ((lpeg.R("01"))^1)
local octInteger = lpeg.P("0") * ((lpeg.R("07"))^1)
local decimalInteger = (lpeg.R("09")^1)
local integer = (lpeg.C(hexInteger + binaryInteger + octInteger + decimalInteger) / tonumber) * whitespace
local number = (float + integer) * whitespace

-- Pattern for additive operators
local opAdd = lpeg.C(lpeg.S"+-") * whitespace

-- Pattern for multiplicative operators
local opMul = lpeg.C(lpeg.S"*/%") * whitespace

-- Pattern for exponential operator
local opExpo = lpeg.C(lpeg.P"^") * whitespace

-- Pattern for unary minus and plus
local opSign = opAdd

-- Pattern for relational operators - order is important
local opRel = lpeg.C(lpeg.P("<=") + lpeg.P(">=") + lpeg.P("<") + lpeg.P(">") + lpeg.P("==") + lpeg.P("!=")) * whitespace

local opAssign = lpeg.P("=") * whitespace


-- Pattern for alphanumeric words with underscores
-- Dont add whitespace for following group
local alphaNumUnderscore = lpeg.R("az", "AZ", "09", "__")
local alphaUnderscore = lpeg.R("az", "AZ", "__")

-- Pattern for separator. Separator is semi-colon - it is a separator as in Pascal and not a terminator as in C
local separator = lpeg.P(";") * whitespace

-- Patterns for block
local OB = lpeg.P("{") * whitespace
local CB = lpeg.P("}") * whitespace


-- Patterns for parenthesis
local OP = lpeg.P("(") * whitespace
local CP = lpeg.P(")") * whitespace

-- Pattern for comma (funcDef and funcCall and expressions)
local comma = lpeg.P(",") * whitespace

-- Patterns for square brackets
local OS = lpeg.P("[") * whitespace
local CS = lpeg.P("]") * whitespace

-- Pattern for print
local PRT = lpeg.P("@") * whitespace

-- List of keywords
local keywords = { "and", "or", "if", "elseif", "else", "while", "do", "function", "local", "new", "return"}
local reserved_words = keywords
local reserved_words_pattern = lpeg.P(false)
for i = 1,#reserved_words do
	reserved_words_pattern = reserved_words_pattern + reserved_words[i]
end
reserved_words_pattern = (reserved_words_pattern) * whitespace

function KEYWORD(token)
	assert(reserved_words_pattern:match(token))
	return lpeg.P(token) *
		(function(s,p)
			-- if followed by alphanumeric or underscore then it is not a keyword otherwise it is a keyword
			if alphaNumUnderscore:match(s,p,p) then
				return false 
			else 
				return true 
			end 
		end)
		* whitespace

end

	
-- Pattern for logical operators
local opLogical = (KEYWORD("and") + KEYWORD("or")) * whitespace

-- Fold functions
local function foldBin(lst)
	local tree = lst[1]
	for i = 2,#lst,2 do
		local op = lst[i]
		tree = {tag = "binaryOp", exp1 = tree, op = op, exp2 = lst[i+1]}
	end
	return tree
end

local function foldUnary(lst)
	-- compile time evaluation of repetitive unary additive operators
	local accum = 1
	for i = 1,#lst-1 do
		if lst[i] == "+" then
			accum = accum
		elseif lst[i] == "-" then
			accum = -accum
		else
			error("SyntaxError: Invalid unary iadditive operator")
		end
	end
	local tree = lst[#lst]
	if accum == 1 then
		return tree
	elseif accum == -1 then
		tree = { tag = "binaryOp", exp1 = tree, op = "*", exp2 = { tag = "numericLiteral", value = -1 }}	
	else
		error("internalError: Chained unary signs have magnitude != 1")
	end
	return tree
end

local function oneBinaryOp(lst)
	local tree = lst[1]
	if #lst == 1 then
		return tree
	elseif #lst == 3 then
		tree = { tag = "binaryOp", exp1 = tree, op = lst[2], exp2 = lst[3] }
	else
		error("SyntaxError: invalid relational expression")
	end
	return tree
end

local function foldIndex(lst)
	local tree = { tag = "arrayIndex", indexTree = nil, index = lst[#lst]}
	for i = (#lst)-1,2,-1 do
		tree = { tag = "arrayIndex", indexTree = tree, index = lst[i]}
	end
	tree = { tag = "array", indexTree = tree, array = lst[1]}
	return tree
end

local function foldNewIndex(lst)
	local tree = { tag = "arrayIndex", indexTree = nil, index = lst[#lst]}
	for i = (#lst)-1,1,-1 do
		tree = { tag = "arrayIndex", indexTree = tree, index = lst[i]}
	end
	tree = { tag = "newarray", indexTree = tree}
	return tree
end


local start_id = -1
local stop_id = -1
ID = lpeg.C(alphaUnderscore *
			lpeg.P(function(s,p) 
				start_id = p
				return true
			end)
			*
			alphaNumUnderscore^0
			*
			function(s,p)
				stop_id = p
				if reserved_words_pattern:match(string.sub(s,start_id, stop_id)) then
					return false
				else
					return true
				end
			end
		) * whitespace


-- Non-terminals
local powerExp = lpeg.V"powerExp"
local unaryExp = lpeg.V"unaryExp"
local multExp = lpeg.V"multExp"
local addExp = lpeg.V"addExp"
local relExp = lpeg.V"relExp"
local rvalue = lpeg.V"rvalue"
local lvalue = lpeg.V"lvalue"
local stat = lpeg.V"stat"
local stats = lpeg.V"stats"
local block = lpeg.V"block"
local ifelseifstat = lpeg.V"ifelseifstat"
local ifelsestat = lpeg.V"ifelsestat"
local ifstat = lpeg.V"ifstat"
local elsestat = lpeg.V"elsestat"
local funcDef = lpeg.V"funcDef"
local funcCall = lpeg.V"funcCall"
local array = lpeg.V"array"
local newarray = lpeg.V"newarray"
local base = lpeg.V"base"
local exponent = lpeg.V"exponent"
local prt = lpeg.V"prt"

PUG = {
	grammar = {}, ast = {}, funcDefs = {}, funcAddr = {}, globals = {}, nglobals = 0, locals = {}, blkNest = 0,
        expOpcodes = { ["+"] = "add", ["-"] = "sub", ["/"] = "div", ["*"] = "mul", ["%"] = "mode", ["^"] = "exp",
	               ["<="] = "leq", ["<"] = "lt", [">="] = "geq", [">"] = "gt", ["=="] = "eq", ["!="] = "neq"},
	stack = {}, mem = {}
      }
function PUG:astNode(tag, ...)
	local lst = table.pack(...) -- generate a list from varargs
	local params_str = table.concat(lst, ", ") -- create a string containing comma separated varargs
        -- create varargs[i] = varargs[i] name value pairs in a string
	local ast_params_str = string.gsub(params_str, "%w+", "%1 = %1")
	local code_str = string.format("return function(%s) return {tag = '%s', %s} end",
			params_str, tag, ast_params_str)
	return load(code_str)()
end

-- Grammar for the pug language.
local grammar = lpeg.P{"prog",
			prog = whitespace
				* lpeg.Ct(funcDef ^ 1) / PUG:astNode("program", "funcDefList")
				* whitespace,
			base = array 
				+ funcCall 
				+ (ID / PUG:astNode("var", "name"))
				+ (number / PUG:astNode("numericLiteral", "value")),
			exponent = array 
				+ funcCall
				+ (ID / PUG:astNode("var", "name")) 
				+ (integer / PUG:astNode("numericLiteral", "value")),
			powerExp = lpeg.Ct(base * ((opExpo * exponent)^0))
					/ foldBin,
			unaryExp = lpeg.Ct(((opSign) ^ 0) * powerExp)
					/ foldUnary,
			multExp = lpeg.Ct((unaryExp * ((opMul * unaryExp)^0)))
					/ foldBin, 
			addExp = lpeg.Ct((multExp * ((opAdd * multExp)^0)))
					/ foldBin,
			relExp = lpeg.Ct(addExp * ((opRel * addExp)^-1))
					/ oneBinaryOp,
			rvalue = newarray
				 + (lpeg.Ct((relExp * ((opLogical * relExp)^0)))
					/ foldBin),
			lvalue = ((KEYWORD("local") * array) / PUG:astNode("lvar", "array")) 
					+ ((KEYWORD("local") * ID) / PUG:astNode("lvar", "name"))
					+ array
					+ (ID / PUG:astNode("var", "name")),
			funcCall = (ID * OP * lpeg.Ct((rvalue * ((comma * rvalue) ^ 0))^0) * CP)
					/ PUG:astNode("funcCall", "funcName", "argsList"),
			array = lpeg.Ct(ID * ((OS * rvalue * CS) ^ 1)) / foldIndex,
			newarray = KEYWORD("new") * lpeg.Ct((OS * rvalue * CS) ^ 1) / foldNewIndex,
                        -- lvalue and rvalue all by themselves are valid statements
			ifelseifstat = 
			                OP 
					* rvalue
					* CP
					* block
					* KEYWORD("elseif") * (ifelseifstat + ifelsestat + ifstat) -- order
					/ PUG:astNode("ifstat", "predExp", "ifBlock", "elseifstat"),
			ifelsestat = 
			                OP 
					* rvalue
					* CP
					* block
					* elsestat
					/ PUG:astNode("ifstat", "predExp", "ifBlock", "elsestat"),
			ifstat = 
			                OP 
					* rvalue
					* CP
					* block
					/ PUG:astNode("ifstat", "predExp", "ifBlock"),
			elsestat = KEYWORD("else") 
					* block
					/ PUG:astNode("elsestat", "elseBlock"),
                        stat = (lvalue * opAssign * rvalue
					/ PUG:astNode("assign", "lvalue", "rvalue"))
				+ KEYWORD("while") * OP
					* rvalue
					* CP
					* block
					/ PUG:astNode("whilestat", "predExp", "loop")
				+ KEYWORD("do")
					* block
					* KEYWORD("while")
					* OP
					* rvalue
					* CP
					/ PUG:astNode("dowhilestat", "loop", "predExp")
				+ KEYWORD("if") * (ifelseifstat + ifelsestat + ifstat)
				+ (PRT * rvalue) / PUG:astNode("prt", "rvalue")
				+ (KEYWORD("return") * (rvalue^0)) / PUG:astNode("returnstat", "rvalue")
			        + rvalue, -- lvalue cannot occur by itself but rvalue can
			stats = lpeg.Ct(stat * ((separator^1) * stat) ^ 0),  -- multiple separators ;;;; are legal
                        -- ; after block is legal
                        -- empty block is legal
			block = OB * stats^-1 * CB * (separator^0) / PUG:astNode("block", "stats"),
			funcDef = (KEYWORD("function")
					* ID 
					* OP * lpeg.Ct((ID * (comma * ID)^0)^0) * CP
					* block
					) / PUG:astNode("funcDef", "funcName", "paramsList", "body")
		}

PUG.grammar = grammar * -1

function PUG:parse(text)
	self.ast = self.grammar:match(text)
	-- tricky - cannot use 'not' here since ast may be a boolean value
	if self.ast == nil then
		print("SyntaxError: Failed to parse pug program")
	end
end

function PUG:codeInstr(opcode)
	if not opcode then
		error("Codegen Error: opcode is nil in instruction code gen")
	end
	if type(opcode) == "table" then
		error("Codegen Error: opcode is a lua `table in instruction code gen")
	end
	local code = self.code
	code[#code + 1] = opcode
end

function PUG:codeImm(num)
	if not num then
		error("Codegen Error: imediate value is nil in imediate value code gen")
	end
	if type(num) == "table" then
		error("Codegen Error: imediate value is a lua table in imediate value code gen")
	end
	local code = self.code
	code[#code + 1] = num
end

function PUG:codeGenIndexTree(ast, dim)
	if ast.tag ~= "arrayIndex" then
		error("CodeGen error - malformed ast - indextree ast has no arrayIndex tag: " .. ast.tag)
	elseif not ast.index then
		error("CodeGen error - malformed ast - indextree ast has no index member")
	else
		self:codeGenExp(ast.index)
		dim = dim + 1
		if ast.indexTree then
			dim = self:codeGenIndexTree(ast.indexTree, dim)
		end
	end
	return dim
end

function PUG:codeGenFuncCall(ast)
	if ast.tag ~= "funcCall" then
		error("CodeGen error - malformed ast - function call ast has no funcCall tag: " .. ast.tag)
	end
	local funcDef = self.funcDefs[ast.funcName]
	if not funcDef then
		error("CodeGen error - function call to undefined function: " .. ast.funcName)
	elseif #funcDef.params ~= #ast.argsList then
		error("CodeGen error - function " .. ast.funcName .."() #args " .. #ast.argsList .. " != #params " .. #funcDef.params) 
	end

	for i = #ast.argsList,1,-1 do
		self:codeGenExp(ast.argsList[i]) -- Generate arguments on stack in reverse order
	end
	for i = 1, #funcDef.locals do
		self:codeInstr("push") -- Push to create space for local parameters
		self:codeImm(0)
	end
	self:codeInstr("call") -- Push to create space for local parameters
	self:codeImm(funcDef.funcAddr)
end
	

function PUG:codeGenExp(ast)
	if ast.tag == "var" then
		-- locals, then function parameters then globals
		local resolved = false
		for i = 1,#self.locals do
			if self.locals[i] == ast.name then
				self:codeInstr("lload")
				self:codeImm(i)
				resolved = true
				break
			end
		end
		if not resolved then
			for i = 1,#self.params do
				if self.params[i] == ast.name then
					self:codeInstr("pload")
					self:codeImm(i)
					resolved = true
					break
				end
			end
		end
		if not resolved then
			if not self.globals[ast.name] then
				error("Codegen error: undefined global: " .. ast.name)
			end
			self:codeInstr("load")
			self:codeImm(self.globals[ast.name])
			resolved = true
		end
	elseif ast.tag == "binaryOp" then
		self:codeGenExp(ast.exp1)
		self:codeGenExp(ast.exp2)
		self:codeInstr(self.expOpcodes[ast.op])
	elseif ast.tag == "newarray" then
		local dim = self:codeGenIndexTree(ast.indexTree, 0)
		self:codeInstr("newarray")
		self:codeImm(dim)
	elseif ast.tag == "funcCall" then
		self:codeGenFuncCall(ast)
	elseif ast.tag == "numericLiteral" then
		self:codeInstr("push")
		self:codeImm(ast.value)
	else 
		error("CodeGen error - malformed ast - unrecognized ast tag: " .. ast.tag)
	end
end

function PUG:codeGenLvalue(ast)
	if ast.tag == "var" then
		if not self.globals[ast.name] then
		    self.nglobals = self.nglobals + 1
		    self.globals[ast.name] = self.nglobals
		end
		self:codeInstr("store")
		self:codeImm(self.globals[ast.name])
	elseif ast.tag == "lvar" then
		self.locals[#self.locals + 1] = ast.name
		self:codeInstr("lstore")
		self:codeImm(#self.locals)
	else

		error("Codegen error: maalformed ast - unrecognized Lvalue tag: " .. ast.tag)
	end


end

function PUG:codeGenGetPCHere()
	return #self.code
end


function PUG:codeGenElseStat(ast)
	if ast.tag ~= "elsestat" then
		error("CodeGenError: missing elsestat ast tag in else statement: " .. ast.tag)
	end
	if not ast.elseBlock then
		error("CodeGenError: missing elseBlock ast member in else statement")
	end
	self:codeGenBlock(ast.elseBlock)
end

function PUG:codeGenIfStat(ast)
	if ast.tag ~= "ifstat" then
		error("CodeGenError: missing ifstat ast tag in if statement: " .. ast.tag)
	end
	if not ast.predExp then
		error("CodeGenError: missing predExp ast member in if statement")
	end
	self:codeGenExp(ast.predExp)
	if not ast.ifBlock then
		error("CodeGenError: missing ifBlock ast member in if statement")
	end
	self:codeInstr("jmpForIfZ")
	self:codeImm(0) -- placeholder for fixup
	local fromPC = self:codeGenGetPCHere()
	self:codeGenBlock(ast.ifBlock)
	self:codeInstr("jmpFor")
	self:codeImm(0) -- placeholder for fixup2
	local fromPC2 = self:codeGenGetPCHere()
	local toPC = self:codeGenGetPCHere()
	self.code[fromPC] = toPC - fromPC + 1
	if not ast.elsestat and not ast.elseifstat then
		;
	elseif ast.elsestat and not ast.elseifstat then
		self:codeGenElseStat(ast.elsestat)
	elseif ast.elseifstat and not ast.elsestat then
		self:codeGenIfStat(ast.elseifstat)
	else
		error("CodeGenError: malformed ifstat ast: " .. pt.pt(ast))
	end
	local toPC2 = self:codeGenGetPCHere()
	print(fromPC2)
	self.code[fromPC2] = toPC2 - fromPC2 + 1
end

function PUG:codeGenStat(ast)
	if ast.tag == "assign" then
		self:codeGenExp(ast.rvalue)
		self:codeGenLvalue(ast.lvalue)
	elseif ast.tag == "ifstat" then
		self:codeGenIfStat(ast)
	elseif ast.tag == "elsestat" then
		self:codeGenElseStat(ast)
	elseif ast.tag == "whilestat" then
		self:codeGenWhileStat(ast)
	elseif ast.tag == "dowhilestat" then
		self:codeGenDoWhileStat(ast)
	elseif ast.tag == "prt" then
		self:codeGenExp(ast.rvalue)
		self:codeInstr("prt")
	elseif ast.tag == "returnstat" then
		if ast.rvalue then
			self:codeGenExp(ast.rvalue)
		end
		self:codeInstr("ret")
		self:codeImm(#self.params + #self.locals) -- pop argument to ret - number of funcparams and locals
	elseif ast.tag == "rvalue" then
		print("Skipping code generation for unassigned rvalue")
	else
		error("Codegen error: maalformed ast - unrecognized statement tag: " .. ast.tag)
	end
	
end

function PUG:codeGenBlock(ast)
	if ast.tag ~= "block" then
		error("CodeGen error: ast is malformed - no block ast tag at block level")
	end
	if not ast.stats then
		return -- empty function body
	end
	for i = 1,#ast.stats do
		self:codeGenStat(ast.stats[i])
	end
end

function PUG:codeGenFuncDef(ast)
	if ast.tag ~= "funcDef" then
		error("CodeGen error: ast is malformed - no funcDef ast tag at function level")
	end
	local funcName = ast.funcName
	self.funcDefs[funcName] = {}
	local funcDef = self.funcDefs[funcName]
	self.funcAddr[#self.funcAddr + 1] = funcName
	print("Generating code for function " ..funcName .. "()")
	funcDef.params = ast.paramsList
	funcDef.code = {}
	funcDef.locals = {}
	funcDef.funcAddr = #self.funcAddr
	-- main cannot take any params
	if funcName == "main" and #funcDef.params ~= 0 then
		error("CodeGenError: main() cannot take any parameters")
	end
	self.params = funcDef.params
	self.code = funcDef.code 
	self.locals = funcDef.locals
	if not ast.body then
		error("CodeGen error: ast is malformed - no body ast tag in function: " .. funcName)
	end
	self:codeGenBlock(ast.body)
	-- code to return a return value for a function in the event it does not have an explicit return
	self:codeInstr("push")
	self:codeImm(0)
	self:codeInstr("ret")
	self:codeImm(#self.params + #self.locals) -- pop argument to ret - number of funcparams and locals
	print(pt.pt(PUG.code))
end
function PUG:codeGen(ast)
	if ast.tag ~= "program" then
		error("CodeGen error: ast is malformed - no program ast tag at root level")
	end
	for i = 1,#ast.funcDefList do
		self:codeGenFuncDef(ast.funcDefList[i])
	end
	-- Note no func call to main since main cannot take parameters in the PUG language
end


function PUG:runArrayAlloc(basearray, top, curdim)
	for i = 1,self.stack[top - curdim + 1] do
		if curdim == 1 then
			basearray[i] = 0
		else
			basearray[i] = {}
			self:runArrayAlloc(basearray[i], top, curdim-1)
		end
	end
end

function PUG:runFunc(funcDef, top)
	local pc = 1
	local code = funcDef.code
	local base = top

	while true do
		if code[pc] == "ret" then
			local retval = self.stack[top]
			top = top - code[pc+1]
			self.stack[top] = retval
			pc = pc + 2
			return top
		elseif code[pc] == "push" then
			top = top + 1
			self.stack[top] = code[pc+1]
			pc = pc + 2
		elseif code[pc] == "call" then
			local funcName = self.funcAddr[code[pc+1]]
			if not funcName then
				error("RuntimeError: undefined function address invoked: " .. code[pc+1])
			else
				print("Running code for function: " .. funcName)
			end
			local funcDef = self.funcDefs[funcName]
			top = self:runFunc(funcDef, top)
			pc = pc + 2
		elseif code[pc] == "pload" then
			value = self.stack[base - #funcDef.locals - code[pc+1] + 1]
			top = top + 1
			self.stack[top] = value
			pc = pc + 2
		elseif code[pc] == "lstore" then
			self.stack[base - code[pc+1] + 1] = self.stack[top]
			top = top - 1
			pc = pc + 2
		elseif code[pc] == "prt" then
			print("PUG stdout: =========> " .. self.stack[top])
			top = top - 1
			pc = pc + 1 
		elseif code[pc] == "lload" then
			top = top + 1
			self.stack[top] = self.stack[base - code[pc+1] + 1]
			pc = pc + 2
		elseif code[pc] == "store" then
			self.mem[code[pc+1]] = self.stack[top]
			top = top - 1
			pc = pc + 2
		elseif code[pc] == "newarray" then
			array = {}
			self:runArrayAlloc(array, top, code[pc+1])
			top = top - code[pc+1]
			top = top + 1
			self.stack[top] = array
			pc = pc + 2
		elseif code[pc] == "add" then
			self.stack[top-1] = self.stack[top - 1] + self.stack[top]
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "sub" then
			self.stack[top-1] = self.stack[top - 1] - self.stack[top]
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "mul" then
			self.stack[top-1] = self.stack[top - 1] * self.stack[top]
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "div" then
			self.stack[top-1] = self.stack[top - 1] / self.stack[top]
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "mod" then
			self.stack[top-1] = self.stack[top - 1] % self.stack[top]
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "exp" then
			self.stack[top-1] = self.stack[top - 1] ^ self.stack[top]
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "geq" then
			if self.stack[top - 1] >= self.stack[top] then 
				self.stack[top-1] = 1
			else
				self.stack[top-1] = 0
			end
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "gt" then
			if self.stack[top - 1] > self.stack[top] then 
				self.stack[top-1] = 1
			else
				self.stack[top-1] = 0
			end
			self.stack[top-1] = self.stack[top - 1] > self.stack[top]
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "leq" then
			if self.stack[top - 1] <= self.stack[top] then 
				self.stack[top-1] = 1
			else
				self.stack[top-1] = 0
			end
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "lt" then
			if self.stack[top - 1] < self.stack[top] then 
				self.stack[top-1] = 1
			else
				self.stack[top-1] = 0
			end
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "eq" then
			if self.stack[top - 1] == self.stack[top] then 
				self.stack[top-1] = 1
			else
				self.stack[top-1] = 0
			end
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "neq" then
			if self.stack[top - 1] ~= self.stack[top] then 
				self.stack[top-1] = 1
			else
				self.stack[top-1] = 0
			end
			top = top - 1
			pc = pc + 1
		elseif code[pc] == "jmpForIfZ" then
			if self.stack[top] == 0 then
				pc = pc + code[pc+1] + 1
			else
				pc = pc + 2
			end
			top = top - 1
		elseif code[pc] == "jmpFor" then
			pc = pc + code[pc+1] + 1
		else
			error("RuntimeError: unsupported opcode: " .. code[pc])
		end
	end
end
	

function PUG:runVM()
	local top = 0

	print("Running code for function: main")
	top = self:runFunc(self.funcDefs["main"], top)
	if top == nil or top ~= 1 then
		error("RuntimeError:  stack top is not 1 on program exit: " .. top)
	end
	print("PUG program ran successfully. Return value to lua interpreter is: " .. tostring(self.stack[1]))
	print("Memory contents after program termination ========> " .. pt.pt(self.mem))
	self.stack = {}
	self.mem = {}
	self.code = {}
end

local input = io.read("*a")
PUG:parse(input)
print(pt.pt(PUG.ast))
PUG:codeGen(PUG.ast)
PUG:runVM()
