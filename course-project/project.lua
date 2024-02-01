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

Compiler = {grammar = {}, ast = {}, funcDefs = {}, funcAddr = {}, globals = {}, nglobals = 0, locals = {}, blkNest = 0,
            expOpcodes = { ["+"] = "add", ["-"] = "sub", ["/"] = "div", ["*"] = "mul", ["%"] = "mode",
	                   ["<="] = "leq", ["<"] = "lt", [">="] = "geq", [">"] = "gt", ["=="] = "eq", ["!="] = "neq"}
	   }
function Compiler:astNode(tag, ...)
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
				* lpeg.Ct(funcDef ^ 1) / Compiler:astNode("program", "funcDefList")
				* whitespace,
			base = array 
				+ funcCall 
				+ (ID / Compiler:astNode("var", "name"))
				+ (number / Compiler:astNode("numericLiteral", "value")),
			exponent = array 
				+ funcCall
				+ (ID / Compiler:astNode("var", "name")) 
				+ (integer / Compiler:astNode("numericLiteral", "value")),
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
			lvalue = ((KEYWORD("local") * array) / Compiler:astNode("lvar", "array")) 
					+ ((KEYWORD("local") * ID) / Compiler:astNode("lvar", "name"))
					+ array
					+ (ID / Compiler:astNode("var", "name")),
			funcCall = (ID * OP * lpeg.Ct((rvalue * ((comma * rvalue) ^ 0))^0) * CP)
					/ Compiler:astNode("funcCall", "funcName", "argsList"),
			array = lpeg.Ct(ID * ((OS * rvalue * CS) ^ 1)) / foldIndex,
			newarray = KEYWORD("new") * lpeg.Ct((OS * rvalue * CS) ^ 1) / foldNewIndex,
                        -- lvalue and rvalue all by themselves are valid statements
			ifelseifstat = 
			                OP 
					* rvalue
					* CP
					* block
					* KEYWORD("elseif") * (ifelseifstat + ifelsestat + ifstat) -- order
					/ Compiler:astNode("ifstat", "predExp", "ifblock", "elseifstat"),
			ifelsestat = 
			                OP 
					* rvalue
					* CP
					* block
					* elsestat
					/ Compiler:astNode("ifistat", "predExp", "ifblock", "elsestat"),
			ifstat = 
			                OP 
					* rvalue
					* CP
					* block
					/ Compiler:astNode("ifstat", "predExp", "ifblock"),
			elsestat = KEYWORD("else") 
					* block
					/ Compiler:astNode("elsestat", "elseblock"),
                        stat = (lvalue * opAssign * rvalue
					/ Compiler:astNode("assign", "lvalue", "rvalue"))
				+ KEYWORD("while") * OP
					* rvalue
					* CP
					* block
					/ Compiler:astNode("whilestat", "predExp", "loop")
				+ KEYWORD("do")
					* block
					* KEYWORD("while")
					* OP
					* rvalue
					* CP
					/ Compiler:astNode("dowhilestat", "loop", "predExp")
				+ KEYWORD("if") * (ifelseifstat + ifelsestat + ifstat)
				+ (PRT * rvalue) / Compiler:astNode("prt", "rvalue")
				+ (KEYWORD("return") * (rvalue^0)) / Compiler:astNode("returnstat", "rvalue")
			        + rvalue, -- lvalue cannot occur by itself but rvalue can
			stats = lpeg.Ct(stat * ((separator^1) * stat) ^ 0),  -- multiple separators ;;;; are legal
                        -- ; after block is legal
                        -- empty block is legal
			block = OB * stats^0 * CB * (separator^0) / Compiler:astNode("block", "stats"),
			funcDef = (KEYWORD("function")
					* ID 
					* OP * lpeg.Ct((ID * (comma * ID)^0)^0) * CP
					* block
					) / Compiler:astNode("funcDef", "funcName", "paramsList", "body")
		}

Compiler.grammar = grammar * -1

function Compiler:parse(text)
	self.ast = self.grammar:match(text)
	-- tricky - cannot use 'not' here since ast may be a boolean value
	if self.ast == nil then
		print("Syntax Error: Failed to parse pug program")
	end
end

function Compiler:codeInstr(opcode)
	if not opcode then
		error("Codegen Error: opcode is nil in instruction code gen")
	end
	if type(opcode) == "table" then
		error("Codegen Error: opcode is a lua `table in instruction code gen")
	end
	local code = self.code
	code[#code + 1] = opcode
end

function Compiler:codeImm(num)
	if not num then
		error("Codegen Error: imediate value is nil in imediate value code gen")
	end
	if type(num) == "table" then
		error("Codegen Error: imediate value is a lua table in imediate value code gen")
	end
	local code = self.code
	code[#code + 1] = num
end

function Compiler:codeGenIndexTree(ast)
	if ast.tag ~= "arrayIndex" then
		error("CodeGen error - malformed ast - indextree ast has no arrayIndex tag: " .. ast.tag)
	elseif not ast.index then
		error("CodeGen error - malformed ast - indextree ast has no index member")
	else
		self:codeGenExp(ast.index)
		if ast.indexTree then
			self:codeGenIndexTree(ast.indexTree)
		end
	end
end

function Compiler:codeGenFuncCall(ast)
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
	for i = 1, #funcDef.params do
		self:codeInstr("push") -- Push to create space for local parameters
		self:codeImm(0)
	end
	self:codeInstr("call") -- Push to create space for local parameters
	self:codeImm(funcDef.funcAddr)
end
	

function Compiler:codeGenExp(ast)
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
		self:codeGenIndexTree(ast.indexTree)
	elseif ast.tag == "funcCall" then
		self:codeGenFuncCall(ast)
	elseif ast.tag == "numericLiteral" then
		self:codeInstr("push")
		self:codeImm(ast.value)
	else 
		error("CodeGen error - malformed ast - unrecognized ast tag: " .. ast.tag)
	end
end


function Compiler:codeGenLvalue(ast)
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

function Compiler:codeGenStat(ast)
	if ast.tag == "assign" then
		self:codeGenExp(ast.rvalue)
		self:codeGenLvalue(ast.lvalue)
	elseif ast.tag == "ifstat" then
		self:codeGenIfstat(ast)
	elseif ast.tag == "elsestat" then
		self:codeGenElsestat(ast)
	elseif ast.tag == "whilestat" then
		self:codeGenWhilestat(ast)
	elseif ast.tag == "dowhilestat" then
		self:codeGenDoWhilestat(ast)
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

function Compiler:codeGenBlock(ast)
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

function Compiler:codeGenFunc(ast)
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
	self.params = funcDef.params
	self.code = funcDef.code 
	self.locals = funcDef.locals
	if not ast.body then
		error("CodeGen error: ast is malformed - no body ast tag in function: " .. funcName)
	end
	self:codeGenBlock(ast.body)
	-- code to return a return value for a function in the even it does not have an explicit return
	self:codeInstr("push")
	self:codeImm(0)
	self:codeInstr("ret")
	self:codeImm(#self.params + #self.locals) -- pop argument to ret - number of funcparams and locals
	print(pt.pt(Compiler.code))
end
function Compiler:codeGen(ast)
	if ast.tag ~= "program" then
		error("CodeGen error: ast is malformed - no program ast tag at root level")
	end
	for i = 1,#(ast.funcDefList) do
		self:codeGenFunc(ast.funcDefList[i])
	end
end

local input = io.read("*a")
Compiler:parse(input)
print(pt.pt(Compiler.ast))
Compiler:codeGen(Compiler.ast)
