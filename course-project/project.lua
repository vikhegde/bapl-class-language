local lpeg = require"lpeg"
local pt = require"pt"

--
local MAXSYNTAXCONTEXT = 20
local MAXSYNTAXLINECONTEXT = 4

-- Pattern for filler text like space and comments - has no capture 
local maxPos = -1
local maxLine = 1 
local space = lpeg.S" \n\t"
local blockcmnt = lpeg.P("#{") * ((lpeg.P(1) - lpeg.P("#}"))^0) * lpeg.P("#}") 
local linecmnt = lpeg.P("#") * ((lpeg.P(1) - lpeg.P("\n"))^0) * lpeg.P("\n") 
local whitespace = ((blockcmnt + linecmnt + space) * lpeg.P(function(s, p)
								maxPos = p
								if string.sub(s,p-1,p-1) == "\n" then
									maxLine = maxLine + 1
								end
								return true
							    end)) ^ 0  -- order is important lpeg is possesive 

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

-- Pattern for terminator. terminator is semi-colon - it is a a terminator as in C and not separator as in Pascal
local terminator = lpeg.P(";") * whitespace

-- Patterns for block
local OB = lpeg.P("{") * whitespace
local CB = lpeg.P("}") * whitespace


-- Patterns for parenthesis
local OP = lpeg.P("(") * whitespace
local CP = lpeg.P(")") * whitespace

-- Pattern for comma (funcDef and funcCall and expressions)
local comma = lpeg.P(",") * whitespace

-- equals is used for default value for last function parameter
local equals = opAssign

-- Patterns for square brackets
local OS = lpeg.P("[") * whitespace
local CS = lpeg.P("]") * whitespace

-- Pattern for print
local PRT = lpeg.P("@") * whitespace

-- Patterns for type subsystem
local indices = lpeg.Ct(lpeg.C(lpeg.P("[]"))^1) * whitespace -- capture needed to count array dimensions
local colon = lpeg.P(":") * whitespace

local function syntaxError(inputText, lineNum, errorPos)
	print("\nsyntaxError: Compile failed at line: " .. tostring(lineNum) .. "\n")
	local contextPrefixPos = -1
	local contextSuffixPos = -1
	local count = 0
	if errorPos - MAXSYNTAXCONTEXT < 1 then
		contextSuffixPos = i
	else
		for i = errorPos-1,errorPos-40,-1 do
			if string.sub(inputText, i, i) == "\n" then
				count = count + 1
				contextPrefixPos = i + 1
				if count == MAXSYNTAXLINECONTEXT then
					break	
				end	
			end
		end
	end

	if count < MAXSYNTAXLINECONTEXT then
		contextPrefixPos = 1 
	end
	count = 0
	if errorPos + 40 > #inputText then
		contextSuffixPos = i
	else
		for i = errorPos, errorPos + MAXSYNTAXCONTEXT do
			if string.sub(inputText, i, i) == "\n" then
				count = count + 1
				contextSuffixPos = i
				if count == MAXSYNTAXLINECONTEXT then
					break
				end
			end
		end
	end
	if count < MAXSYNTAXLINECONTEXT then
		contextSuffixPos = #inputText 
	end
	print(string.sub(inputText, contextPrefixPos, errorPos-1) .. " [<== parse failed HERE ==>] " .. string.sub(inputText, errorPos, contextSuffixPos))
	print("Compile failed")
end

-- List of keywords
local keywords = { "if", "elseif", "else", "while", "do", "function", "local", "new", "return", "num", "arrayof", "void"}
local reserved_words = keywords
local reserved_words_pattern = lpeg.P(false)
for i = 1,#reserved_words do
	reserved_words_pattern = reserved_words_pattern + reserved_words[i]
end
reserved_words_pattern = (reserved_words_pattern) * -1

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
local opLogical = lpeg.C(lpeg.P("&&") + "||") * whitespace

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
			error("syntaxError: Invalid unary iadditive operator")
		end
	end
	local tree = lst[#lst]
	if accum == 1 then
		return tree
	elseif accum == -1 then
		tree = { tag = "binaryOp", exp1 = tree, op = "*", exp2 = { tag = "numericLiteral", value = -1 }}	
	else
		error("PUG parser: internalError: Chained unary signs have magnitude != 1")
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
		error("syntaxError: invalid relational expression")
	end
	return tree
end

local function foldIndex(lst)
	local index = lst[#lst]
	local tree = { tag = "arrayIndex", indexTree = nil, index = index}
	for i = (#lst)-1,2,-1 do
		index = lst[i]
		tree = { tag = "arrayIndex", indexTree = tree, index = index}
	end
	tree = { tag = "array", indexTree = tree, array = lst[1]}
	return tree
end

local function foldNewIndex(lst)
	local index = lst[#lst]
	local tree = { tag = "arrayIndex", indexTree = nil, index = index}
	for i = (#lst)-1,1,-1 do
		index = lst[i]
		tree = { tag = "arrayIndex", indexTree = tree, index = index}
	end
	tree = { tag = "newarray", indexTree = tree}
	return tree
end

local function foldNumType(lst)
	local tree = { tag = "type", typeval = "num"}
	return tree
end

local function foldVoidType(lst)
	local tree = { tag = "type", typeval = "void"}
	return tree
end

local function foldArrayType(lst)
	local tree = { tag = "type", typeval = "num" .. table.concat(lst)}
	return tree
end

local start_id = -1
local stop_id = -1
ID = lpeg.C(alphaUnderscore *
			lpeg.P(function(s,p) 
				start_id = p - 1
				return true
			end)
			*
			alphaNumUnderscore^0
			*
			function(s,p)
				stop_id = p - 1
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
local ifstat = lpeg.V"ifstat"
local elseifstat = lpeg.V"elseifstat"
local elsestat = lpeg.V"elsestat"
local funcDef = lpeg.V"funcDef"
local funcCall = lpeg.V"funcCall"
local array = lpeg.V"array"
local newarray = lpeg.V"newarray"
local base = lpeg.V"base"
local exponent = lpeg.V"exponent"
local prt = lpeg.V"prt"
local typevalue = lpeg.V"typevalue"

PUG = {
	grammar = {}, ast = {}, funcDefs = {}, funcAddr = {}, globals = {}, nglobals = 0, globalsTypes = {},
	localsCountList = {},
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
			base = OP * rvalue * CP
			        + array 
				+ funcCall 
				+ (ID / PUG:astNode("var", "name"))
				+ (number / PUG:astNode("numericLiteral", "value")),
			exponent = OP * rvalue * CP
			        + array 
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
			typevalue = ((KEYWORD("num") * indices) / foldArrayType)
					+ (KEYWORD("num") / foldNumType)
					+ (KEYWORD("void") / foldVoidType),
			-- a parenthesized lvalue is not legal
			lvalue = 	((KEYWORD("local") * typevalue * ID) / PUG:astNode("lvar", "decltype", "name"))
					+ (typevalue * ID / PUG:astNode("var", "decltype", "name"))
					+ (array / PUG:astNode("array", "array")) 
					+ (ID / PUG:astNode("var", "name")),
			funcCall = (ID * OP * lpeg.Ct((rvalue * ((comma * rvalue) ^ 0))^0) * CP)
					/ PUG:astNode("funcCall", "funcName", "argsList"),
			array = lpeg.Ct(ID * ((OS * rvalue * CS) ^ 1)) / foldIndex,
			newarray = KEYWORD("new") * lpeg.Ct((OS * rvalue * CS) ^ 1) / foldNewIndex,
                        -- lvalue and rvalue all by themselves are valid statements
			ifstat =        OP 
					* rvalue
					* CP
					* block
					/ PUG:astNode("ifstat", "predExp", "ifBlock"),
			elsestat = (KEYWORD("elseif") * ifstat * (elsestat^-1) / PUG:astNode("ifelsestat", "ifstat", "nestedelseifstat"))
			           + (KEYWORD("else") 
					* block
					/ PUG:astNode("elsestat", "elseBlock")),
                        stat = ((lvalue * opAssign * rvalue
					/ PUG:astNode("assign", "lvalue", "rvalue"))
				+ KEYWORD("while") * OP
					* rvalue
					* CP
					* block
					/ PUG:astNode("whilestat", "predExp", "whileBlock")
				+ KEYWORD("do")
					* block
					* KEYWORD("while")
					* OP
					* rvalue
					* CP
					/ PUG:astNode("dowhilestat", "whileBlock", "predExp")
				+ KEYWORD("if") * ifstat * (elsestat^-1)  / PUG:astNode("ifelsestat", "ifstat", "nestedelseifstat")
				+ (PRT * rvalue) / PUG:astNode("prt", "rvalue")
				+ (KEYWORD("return") * (rvalue^0)) / PUG:astNode("returnstat", "rvalue")
				 -- lvalue cannot occur by itself but rvalue can provided it is cast to void
			        + (OP * KEYWORD("void") * CP * rvalue) / PUG:astNode("voidstat", "rvalue")
			        + rvalue / PUG:astNode("rvaluestat", "rvalue")
				+ block -- nested block
				)  -- empty statements are valid
				* (terminator^1) + (terminator^1),   -- multiple terminators ;;;; are legal
			stats = lpeg.Ct(stat ^ 0),
                        -- ; after function-body-block is not legal. ; after if-block is legal
                        -- empty block is legal
			block = OB * stats^-1 * CB / PUG:astNode("block", "stats"),
			funcDef = (typevalue * KEYWORD("function")
					* ID 
					* OP * lpeg.Ct((ID * colon * typevalue * (comma * ID * colon * typevalue)^0)^0) 
					* (equals * rvalue)
					* CP
					* block
					) / PUG:astNode("funcDef", "retType", "funcName", "paramsList", "defaultVal", "body")
				   + ((typevalue * KEYWORD("function")
					* ID 
					* OP * lpeg.Ct((ID * colon * typevalue * (comma * ID * colon * typevalue)^0)^0) 
					* (equals * rvalue)
					* CP
					)/ PUG:astNode("funcDef", "retType", "funcName", "paramsList", "defaultVal"))
				   + ((typevalue * KEYWORD("function")
					* ID 
					* OP * lpeg.Ct((ID * colon * typevalue * (comma * ID * colon * typevalue)^0)^0) * CP
					* block
					) / PUG:astNode("funcDef", "retType", "funcName", "paramsList", "body"))
				   +  ((typevalue * KEYWORD("function")
					* ID 
					* OP * lpeg.Ct((ID * colon * typevalue * (comma * ID * colon * typevalue)^0)^0) * CP
					) / PUG:astNode("funcDef", "retType", "funcName", "paramsList"))

		}

PUG.grammar = grammar * -1

function PUG:parse(inputText)
	self.ast = self.grammar:match(inputText)
	-- tricky - cannot use 'not' here since ast may be a boolean value
	if self.ast == nil then
		syntaxError(inputText, maxLine, maxPos)
		os.exit()
	end
end

function PUG:codeInstr(opcode)
	if not opcode then
		error("codeGenError: opcode is nil in instruction code gen")
	end
	if type(opcode) == "table" then
		error("codeGenError: opcode is a lua `table in instruction code gen")
	end
	local code = self.code
	code[#code + 1] = opcode
end

function PUG:codeImm(num)
	if not num then
		error("codeGenError: immediate value is nil in immediate value code gen")
	end
	if type(num) == "table" then
		error("codeGenError: immediate value is a lua table in immediate value code gen")
	end
	local code = self.code
	code[#code + 1] = num
end

function PUG:codeGenIndexTree(ast, dim)
	if ast.tag ~= "arrayIndex" then
		error("syntaxError - malformed AST - indextree ast has no arrayIndex tag: " .. ast.tag)
	elseif not ast.index then
		error("syntaxError - malformed AST - indextree ast has no index member")
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
		error("syntaxError - malformed AST - function call ast has no funcCall tag: " .. ast.tag)
	end
	local funcDef = self.funcDefs[ast.funcName]
	if not funcDef then
		error("declarationError - function call to undefined function: " .. ast.funcName)
	elseif (#funcDef.paramsTypes["params"] ~= #ast.argsList) and (not funcDef.defaultAst or #ast.argsList ~= #funcDef.paramsTypes["params"] - 1) then
		error("typeCheckError - function " .. ast.funcName .."() #args " .. #ast.argsList .. " != #params " .. #funcDef.paramsTypes["params"]) 
	end

	if #ast.argsList == #funcDef.paramsTypes["params"] - 1 then
		assert(funcDef.defaultAst)
		self:codeGenExp(funcDef.defaultAst) -- generate final argument with default value in stack (if needed)
	end
	for i = #ast.argsList,1,-1 do
		self:codeGenExp(ast.argsList[i]) -- Generate arguments on stack in reverse order
	end
	for i = 1, #funcDef.locals do
		self:codeInstr("push") -- Push to create space for local parameters
		self:codeImm(0)
	end
	self:codeInstr("call")
	self:codeImm(funcDef.funcAddr)
	return funcDef.retType
end
	

function PUG:codeGenExp(ast)
	local retType = "unknownType"
	if ast.tag == "var" then
		-- locals, then function parameters then globals
		local resolved = false
		-- reverse order to access nearest scope
		for i = #self.locals,1,-1 do
			if self.locals[i] == ast.name then
				self:codeInstr("lload")
				self:codeImm(i)
				resolved = true
				retType = self.localsTypes[i]
				break
			end
		end
		if not resolved then
			for i = 1,#self.paramsTypes["params"] do
				if self.paramsTypes["params"][i] == ast.name then
					self:codeInstr("pload")
					self:codeImm(i)
					resolved = true
					retType = self.paramsTypes["types"][i]
					break
				end
			end
		end
		if not resolved then
			if not self.globals[ast.name] then
				error("declarationError: undefined global: " .. ast.name)
			end
			self:codeInstr("load")
			self:codeImm(self.globals[ast.name])
			resolved = true
			retType = self.globalsTypes[ast.name]
		end
	elseif ast.tag == "binaryOp" then
		local ret1 = self:codeGenExp(ast.exp1)
		print(pt.pt(ast))
		local ret2 = self:codeGenExp(ast.exp2)
		if ret1 ~= ret2 then
			error("TypeCheck: type mismatch between LHS (" .. ret1 .. ") and RHS (" .. ret2 .. ") of binary op: " .. ast.op)
		else
			print("typecheck success")
		end
		self:codeInstr(self.expOpcodes[ast.op])
		retType = ret1
	elseif ast.tag == "newarray" then
		local dim = self:codeGenIndexTree(ast.indexTree, 0)
		self:codeInstr("newarray")
		self:codeImm(dim)
		retType = "num"
		for i = 1, dim do
			retType = retType .. "[]"
		end
	elseif ast.tag == "funcCall" then
		retType = self:codeGenFuncCall(ast)
	elseif ast.tag == "numericLiteral" then
		self:codeInstr("push")
		self:codeImm(ast.value)
		return "num"
	elseif ast.tag == "array" then
		if not ast.array or not ast.indexTree then
			error("syntaxError - malformed AST - missing array or indexTree element in rvalue array ast")
		end
		local dim = self:codeGenIndexTree(ast.indexTree, 0)
		local resolved = false
		for i = 1,#self.locals do
			if self.locals[i] == ast.array then
				self:codeInstr("larrayget")
				self:codeImm(i)
				resolved = true
				retType = self.localsTypes[i]
				break
			end
		end
		if not resolved then
			local gptr = self.globals[ast.array]
			if not gptr then
				error("declarationError - failed to resolve array name in array rvalue: " .. ast.array)
			end
			self:codeInstr("arrayget")
			self:codeImm(gptr)
			retType = self.globalsTypes[ast.array]
		end
		self:codeImm(dim)
		while dim >= 1 do
			if string.sub(retType,#retType-1,#retType) == "[]" then
				retType = string.sub(retType, 1, #retType-2)
			elseif retType == "num" then
				error("typeCheckError: attempting to apply index to a number")
			else
				error("typeCheckError: attempting to apply index to a non-array type")
			end
			dim = dim - 1
		end
	else 
		error("syntaxError - malformed AST - unrecognized ast tag: " .. ast.tag)
	end
	if retType == "unknownType" or retType == nil then
		error("typeCheckError: expression has no type: " .. pt.pt(ast))
	end
	return retType
end

function PUG:codeGenLvalue(ast)
	local retType = "unknownType"
	if ast.tag == "var" then
		if ast["decltype"] then
			if ast.name then
				if self.globals[ast.name] then
					error("declarationnError; new type declaration found for a global that was declared previously")
				elseif string.sub(ast["decltype"].typeval, #ast["decltype"].typeval - 1, #ast["decltype"].typeval) == "[]" then
					local arraytype = ast["decltype"].typeval
					local dim = 0
					for i = 4,#arraytype,2 do
						local subscript = string.sub(arraytype,i,i+1)
						if  subscript ~= "[]" then
							error("typeCheckError: type declaration for array has non-subscript operator" .. subscript)
						end
						dim = dim + 1
					end
		    			self.nglobals = self.nglobals + 1
		    			self.globals[ast.name] = self.nglobals
					retType = ast["decltype"].typeval
					self.globalsTypes[ast.name] = retType 
					local gptr = self.nglobals
					self:codeInstr("arrayalloc")
					self:codeImm(gptr)
					self:codeImm(dim)
					retType = self.globalsTypes[ast.name]
				else
		    			self.nglobals = self.nglobals + 1
		    			self.globals[ast.name] = self.nglobals
					self.globalsTypes[ast.name] = ast["decltype"].typeval
					retType = self.globalsTypes[ast.name]
					self:codeInstr("store")
					self:codeImm(self.globals[ast.name])
				end
			else
				error("typeCheckError: new typed declaration found for a global with no name ast element")
			end
		else
			if ast.name then
				local resolved = false
				for i = #self.locals,1,-1 do
					if self.locals[i] == ast.name then
						self:codeInstr("lstore")
						self:codeImm(i)
						resolved = true
						retType = self.localsTypes[i]
						break
					end
				end
				if not resolved then
					for i = 1,#self.paramsTypes["params"] do
						if self.paramsTypes["params"][i] == ast.name then
							self:codeInstr("pstore")
							self:codeImm(i)
							resolved = true
							retType = self.paramsTypes["types"][i]
							break
						end
					end
				end
				if not resolved and not self.globals[ast.name] then
					error("declarationError: reference to lvalue global that was not declared previously")
				elseif not resolved then
					self:codeInstr("store")
					self:codeImm(self.globals[ast.name])
					retType = self.globalsTypes[ast.name]
				end
			else
				error("syntaxError: malformed AST: lvalue global var ast with no name ast element")
			end
		end
	elseif ast.tag == "lvar" then
		if ast["decltype"] then
			if ast.name then
				for i = self.localsCountsList[#self.localsCountsList],1,-1 do
					if self.locals[i] == ast.name then
					    error("declarationError: new type declaration found for a local that was declared previously in same scope")
					    break
					end
				end
				if string.sub(ast["decltype"].typeval, #ast["decltype"].typeval - 1, #ast["decltype"].typeval) == "[]" then
					local arraytype = ast["decltype"].typeval
					local dim = 0
					for i = 4,#arraytype,2 do
						local subscript = string.sub(arraytype,i,i+1)
						if  subscript ~= "[]" then
							error("typeCheckError: type declaration for array has non-subscript operator" .. subscript)
						end
						dim = dim + 1
					end
					self.locals[#self.locals + 1] = ast.name
					self.localsTypes[#self.locals] = ast["decltype"].typeval
					self.localsCountsList[#self.localsCountsList] 
						= self.localsCountsList[#self.localsCountsList] + 1
					retType = ast["decltype"].typeval
					self.localsTypes[ast.name] = retType 
					self:codeInstr("larrayalloc")
					self:codeImm(#self.locals)
					self:codeImm(dim)
					retType = self.localsTypes[ast.name]
				else
					self.locals[#self.locals + 1] = ast.name
					self.localsTypes[#self.locals] = ast["decltype"].typeval
					self.localsCountsList[#self.localsCountsList] 
						= self.localsCountsList[#self.localsCountsList] + 1
					self:codeInstr("lstore")
					self:codeImm(#self.locals)
					retType = self.localsTypes[#self.locals] 
				end
			else
				error("syntaxError: malformed AST: new typed declaration for a local with no name ast element")
			end
		else
			error("syntaxError: malformed AST: lvalue lvar ast with no decltype element")
			--[[
			if ast.name then
				self:codeInstr("lstore")
				self:codeImm(#self.locals)
				retType = self.localsTypes[ast.name] 
			else
				error("syntaxError: malformed AST: lvalue local var ast with no name ast element")
			end
			--]]
		end
	elseif ast.tag == "array" then
		if not ast.indexTree then
			ast = ast.array
		end
		if not ast.indexTree then
			error("syntaxError: malformed AST: no indexTree element in array ast")
		end
		local dim = self:codeGenIndexTree(ast.indexTree, 0)
		local resolved = false
		for i = 1,#self.locals do
			if self.locals[i] == ast.array then
				self:codeInstr("larrayset")
				self:codeImm(i)
				resolved = true
				retType = self.localsTypes[i]
				break
			end
		end
		if not resolved then
			for i = 1,#self.paramsTypes["params"] do
				if self.paramsTypes["params"][i] == ast.array then
					error("typeCheckError: array name (" .. ast.array ..") matches a function parameter. Arrays are not allowed as function parameters")
				end
			end
		end
		if not resolved then
			local gptr = self.globals[ast.array]
			if not gptr then
				error("declarationError: failed to resolve array name in array lvalue: " .. ast.array)
			end
			self:codeInstr("arrayset")
			self:codeImm(gptr)
			retType = self.globalsTypes[ast.array]
		end
		self:codeImm(dim)
		while dim >= 1 do
			if string.sub(retType,#retType-1,#retType) == "[]" then
				retType = string.sub(retType, 1, #retType-2)
			elseif retType == "num" then
				error("typeCheckError: attempting to apply index to a number")
			else
				error("typeCheckError: attempting to apply index to a non-array type")
			end
			dim = dim - 1
		end
	else

		error("syntaxError: malformed AST - unrecognized Lvalue tag: " .. ast.tag)
	end
	return retType
end

function PUG:codeGenGetPCHere()
	return #self.code
end

function PUG:codeGenWhileStat(ast)
	if ast.tag ~= "whilestat" then
		error("syntaxError: malformed AST: missing whilestat ast tag in while statement: " .. ast.tag)
	end
	if not ast.predExp then
		error("syntaxError: malformed AST: missing predExp ast member in while statement")
	end
	if not ast.whileBlock then
		error("syntaxError: malformed AST: missing whileBlock ast member in while statement")
	end
	local toPC2 = self:codeGenGetPCHere()
	self:codeGenExp(ast.predExp)
	self:codeInstr("jmpIfZ")
	self:codeImm(0) -- placeholder for fixup
	local fromPC = self:codeGenGetPCHere()
	self:codeGenBlock(ast.whileBlock)
	self:codeInstr("jmp")
	self:codeImm(0) -- placeholder for fixup2
	local toPC = self:codeGenGetPCHere()
	self.code[fromPC] = toPC + 1 - fromPC -- skip while block
	local fromPC2 = toPC
	self.code[fromPC2] = toPC2 + 1 - fromPC2
	return "void"
end

function PUG:codeGenDoWhileStat(ast)
	if ast.tag ~= "dowhilestat" then
		error("syntaxError: malformed AST: missing dowhilestat ast tag in do-while statement: " .. ast.tag)
	end
	if not ast.predExp then
		error("syntaxError: malformed AST: missing predExp ast member in do-while statement")
	end
	if not ast.whileBlock then
		error("syntaxError: malformed AST: missing while-Block ast member in do-while statement")
	end
	local toPC = self:codeGenGetPCHere()
	self:codeGenBlock(ast.whileBlock)
	self:codeGenExp(ast.predExp)
	self:codeInstr("jmpIfNZ")
	local fromPC = self:codeGenGetPCHere()
	self:codeImm(toPC + 1 - (fromPC + 1))
	return "void"
end

function PUG:codeGenElseStat(ast)
	if ast.tag ~= "elsestat" then
		error("syntaxError: malformed AST: missing elsestat ast tag in else statement: " .. ast.tag)
	end
	if not ast.elseBlock then
		error("syntaxError: malformed AST: missing elseBlock ast member in else statement")
	end
	return self:codeGenBlock(ast.elseBlock)
end

function PUG:codeGenIfElseStat(ast)
	if ast.tag ~= "ifelsestat" then
		error("syntaxError: malformed AST: missing ifelsestat ast tag in if statement: " .. ast.tag)
	end
	if not ast.ifstat or not ast.ifstat.predExp then
		error("syntaxError: malformed AST: missing ifstat or ifstat.predExp ast member in if statement")
	end
	self:codeGenExp(ast.ifstat.predExp)
	if not ast.ifstat.ifBlock then
		error("syntaxError: malformed AST: missing ifBlock ast member in if statement")
	end
	self:codeInstr("jmpIfZ")
	self:codeImm(0) -- placeholder for fixup
	local fromPC = self:codeGenGetPCHere()
	self:codeGenBlock(ast.ifstat.ifBlock)
	self:codeInstr("jmp")
	self:codeImm(0) -- placeholder for fixup2
	local fromPC2 = self:codeGenGetPCHere()
	local toPC = self:codeGenGetPCHere()
	self.code[fromPC] = toPC + 1 - fromPC
	if ast.nestedelseifstat then
		self:codeGenStat(ast.nestedelseifstat)
	end
	local toPC2 = self:codeGenGetPCHere()
	self.code[fromPC2] = toPC2 + 1 - fromPC2 
	return "void"
end

--[[
function PUG:codeGenVar(ast)
	if ast.tag ~= "var" then
		error("syntaxError: malformed AST:  var ast tag not found in var code statement: " .. ast.tag)
	end
	local retType = self:codeGenExp(ast)
	self:codeInstr("pop")
	self:codeImm(1)
	return retType
end
--]]

function PUG:codeGenStat(ast)
	local retType = "unknownType"
	if ast.tag == "assign" then
		local ret1 = self:codeGenExp(ast.rvalue)
		local ret2 = self:codeGenLvalue(ast.lvalue)
		if ret1 ~= ret2 then
			-- NOTE ret2 is LHS and ret1 is RHS
			error("typeCheckError: assignment: LHS type (" .. ret2 .. ") != RHS type (" .. ret1 .. ")")
		end
		retType = "void"
	elseif ast.tag == "ifelsestat" then
		retType = self:codeGenIfElseStat(ast)
		if retType ~= "void" then
			error("typeCheckError: statement return type should be void")
		end
	elseif ast.tag == "elsestat" then
		retType = self:codeGenElseStat(ast)
		if retType ~= "void" then
			error("typeCheckError: statement return type should be void")
		end
	elseif ast.tag == "whilestat" then
		retType = self:codeGenWhileStat(ast)
		if retType ~= "void" then
			error("typeCheckError: statement return type should be void")
		end
	elseif ast.tag == "dowhilestat" then
		retType = self:codeGenDoWhileStat(ast)
		if retType ~= "void" then
			error("typeCheckError: statement return type should be void")
		end
	elseif ast.tag == "prt" then
		if ast.rvalue == nil then
			error("syntaxError: print statement with no rvalue")
		end
		local expType = self:codeGenExp(ast.rvalue)
		if expType == "void" then
			error("typeCheckError: print statement with void rvalue: " .. pt.pt(ast.rvalue))
		end	
		self:codeInstr("prt")
		retType = "void"
	elseif ast.tag == "returnstat" then
		if ast.rvalue then
			local expType = self:codeGenExp(ast.rvalue)
			if expType ~= self.retType then
				error("typeCheckError: function returntype (" .. self.retType .. ") != return expression type (" .. expType .. ")")
			end

			if self.retType == "void" and expType ~= "void" then
				error("typeCheckError: function returns void but return statement has a non-void return value")
			end
			retType = expType
		else
			retType = "void"
		end
		self:codeInstr("ret")
		-- pop argument to ret - number of funcparams.
		--locals freed by block exit
		self:codeImm(#self.paramsTypes["params"]) 
	elseif ast.tag == "var" then
		error("This code should not be reached")
		--[[
		local expType = self:codeGenVar(ast)
		if expType == nil then
			error("typeCheckError: rvalue has no type" .. pt.pt(ast.rvalue))
		end
		retType = "void"
		--]]
	elseif ast.tag == "voidstat" then
		if ast.rvalue == nil then
			error("typeCheckError: cast to void for nonexistent rvalue")
		end
		local expType = self:codeGenExp(ast.rvalue)
		if expType == nil then
			error("typeCheckError: rvalue has no type" .. pt.pt(ast.rvalue))
		end
		if expType ~= "void" then
			self:codeInstr("pop")
			self:codeImm(1)
		end
		retType = "void"
	elseif ast.tag == "rvaluestat" then
		-- valid only if rvalue evaluates to void type
		if ast.rvalue == nil then
			error("syntaxError: rvalue statement has nonexistent rvalue")
		end
		local expType = self:codeGenExp(ast.rvalue)
		if expType == nil then
			error("typeCheckError: rvalue has no type" .. pt.pt(ast.rvalue))
		end
		if expType ~= "void" then
			error("typeCheckError: rvalue-statement (not same as a void-cast-rvalue-statement) is only valid if it is is of type void: type " .. expType)
		end
		retType = expType
	elseif ast.tag == "block" then
		retType = self:codeGenBlock(ast)
		if retType ~= "void" then
			error("typeCheckError: block type != void: type " .. expType)
		end
	else
		error("syntaxError: malformed AST - unrecognized statement ast: " .. pt.pt(ast.tag))
	end
	return retType
	
end

function PUG:codeGenBlock(ast)
	if ast.tag ~= "block" then
		error("syntaxError: AST is malformed - no block ast tag at block level")
	end
	if not ast.stats then
		return -- empty function body
	end
	self.localsCountsList[#self.localsCountsList + 1] = 0
	for i = 1,#ast.stats do
		local retType = self:codeGenStat(ast.stats[i])
		if retType ~= "void" and retType ~= self.retType then
			error("typeCheckError: return statement value type (" .. retType .. ") != function return type (" .. self.retType .. ")")
		elseif self.retType == "void" and retType ~= "void" then 
			error("typeCheckError: return statement value type (" .. retType .. ") != function return type (" .. self.retType .. ")")
		end
	end
	-- pop locals for this block (includes function blocks)
	for i = 1,self.localsCountsList[#self.localsCountsList] do
		self.locals[i] = nil
		self.localsTypes[i] = nil
	end
	self.localsCountsList[#self.localsCountsList] = nil
	-- block returns void type
	return "void"
end

function PUG:codeGenFuncDefBody(ast)
	local retType = self:codeGenBlock(ast.body)
	-- code to return a return value for a function in the event it does not have an explicit return
	if self.retType ~= "void" then
		self:codeInstr("push")
		self:codeImm(0)
	end
	self:codeInstr("ret")
 	-- pop argument to ret - number of funcparams
	-- locals freed by block exit
	self:codeImm(#self.paramsTypes["params"])
	print(pt.pt(PUG.code))
	return retType
end

function PUG:foldParamsTypes(lst)
	local paramsTypes = {}
	paramsTypes["params"] = {}
	paramsTypes["types"] = {}
	if #lst ~= 0 then
		paramsTypes["params"][#paramsTypes["params"] + 1] = lst[1]
		paramsTypes["types"][#paramsTypes["types"] + 1] = lst[2].typeval
		for i = 3,#lst,2 do
			paramsTypes["params"][#paramsTypes["params"] + 1] = lst[i]
			paramsTypes["types"][#paramsTypes["types"] + 1] = lst[i+1].typeval
		end
	end
	if #paramsTypes["params"] ~= #paramsTypes["types"] then
		error("typeCheckError: number of parameters and their types is not equal")
	end
	return paramsTypes
end

function PUG:codeGenFuncDef(ast)
	if ast.tag ~= "funcDef" then
		error("syntaxError: AST is malformed - no funcDef ast tag at function level")
	end
	local funcName = ast.funcName
	self.funcName = funcNamw
	local funcDef = self.funcDefs[funcName]
	if funcDef then
		if funcDef.isProto then
			print("Detected function defintion for function prototype. Verifying return type and number of parameters match...")
			if funcDef.retType ~= ast.retType.typeval then
				error("typeCheckError: mismatch between previous function prototype return type and function definition: " .. funcName)
			end
			if (2 * #funcDef.paramsTypes["params"] ~= #ast.paramsList) or (funcDef.defaultAst == nil and ast.defaultVal)
				or (funcDef.defaultAst and ast.defaultVal == nil) then
				error("typeCheckError: mismatch between number and/or type(s) of parameters of previous function prototype of function and function definition: " .. funcName)
			end
			if not ast.body then
				print("Detected multiple compatible function proototypes for function: " .. funcName .. " Ignoring...")
				return
			end
			self.paramsTypes = funcDef.paramsTypes
			self.code = funcDef.code 
			self.locals = funcDef.locals
			self.localsTypes = funcDef.localsTypes
			self.localsCountsList = funcDef.localsCountsList
			self.retType = funcDef.retType
			self:codeGenFuncDefBody(ast)
			return
		elseif ast.body then
			error("typeCheckError: multiple definition of function definition: " .. funcName)
		else
			print("Detected compatible function proototype for function: " .. funcName .. " after function definition.")
			print("Verifying return type and number of parameters...")
			if funcDef.retType ~= ast.retType then
				error("typeCheckError: mismatch between previous function prototype return type and function definition: " .. funcName)
			end
			if #funcDef.paramsTypes["params"] ~= #ast.paramsList or (funcDef.defaultAst == nil and ast.defaultVal)
				or (funcDef.defaultAst and ast.defaultVal == nil) then
				error("typeCheckError: mismatch between previous function defintion of function and function prototype: " .. funcName)
			end
			print("Ignoring redundant function prototype for function: " .. funcName .. " after function definition.")
			return
		end
	end
			
	self.funcDefs[funcName] = {}
	local funcDef = self.funcDefs[funcName]
	self.funcAddr[#self.funcAddr + 1] = funcName
	print("Generating code for function " ..funcName .. "()")
	funcDef.retType = ast.retType.typeval 
	funcDef.paramsTypes = self:foldParamsTypes(ast.paramsList)
	local hashTable = {}
	for i = 1,#funcDef.paramsTypes["params"] do
		if hashTable[funcDef.paramsTypes["params"][i]] then
			error("declarationError: function parameter '" .. funcDef.paramsTypes["params"][i] 
				.. "' is repeated in parameter list of function: " .. funcName)
		end
		hashTable[funcDef.paramsTypes["params"][i]] = true
		local pType = funcDef.paramsTypes["types"][i]
		if string.sub(pType, #pType-1, #pType) == "[]" then
			error("typeCheckError: function parameters cannot be of array type")
		end
	end
	if ast.defaultVal then
		funcDef.defaultAst = ast.defaultVal
	end
	funcDef.code = {}
	funcDef.locals = {}
	funcDef.localsTypes = {}
	funcDef.localsCountsList = {}
	funcDef.funcAddr = #self.funcAddr
	-- main cannot take any params
	if funcName == "main" and #funcDef.paramsTypes["params"] ~= 0 then
		error("typeCheckError: main() cannot take any parameters")
	elseif funcName == "main" and funcDef.retType == "void" then
		error("typeCheckError: main() cannot return void")
	elseif funcName == "main" and funcDef.retType ~= "num" then
		error("typeCheckError: main() can only return type num and not " .. funcDef.retType)
	end
	self.paramsTypes = funcDef.paramsTypes
	self.code = funcDef.code 
	self.locals = funcDef.locals
	self.localsTypes = funcDef.localsTypes
	self.localsCountsList = funcDef.localsCountsList
	self.retType = funcDef.retType
	if not ast.body then
		print("Detected function prototype for function: " .. funcName)
		funcDef.isProto = true
		return
	end
	self:codeGenFuncDefBody(ast)
	return "void" 
end
function PUG:codeGen(ast)
	if ast.tag ~= "program" then
		error("syntaxError: AST is malformed - no program ast tag at root level")
	end
	local hasMain = false
	for i = 1,#ast.funcDefList do
		self:codeGenFuncDef(ast.funcDefList[i])
		if self.funcDefs["main"] then
			hasMain = true
		end
	end
	if not hasMain then
		error("syntaxError: program has no main() function")
	end

	-- Note no func call to main since main cannot take parameters in the PUG language
	for g,_ in pairs(self.globals) do
		if self.funcDefs[g] then
			error("declarationError: function and global variable have same name: " .. g)
		end
	end
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
		elseif code[pc] == "push" then -- argument to push is value to push on stack
			top = top + 1
			self.stack[top] = code[pc+1]
			pc = pc + 2
		elseif code[pc] == "pop" then -- argument to pop is number of values to pop
			top = top - code[pc+1]
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
		elseif code[pc] == "pstore" then
			self.stack[base - #funcDef.locals - code[pc+1] + 1] = self.stack[top]
			top = top - 1
			pc = pc + 2
		elseif code[pc] == "lstore" then
			self.stack[base - code[pc+1] + 1] = self.stack[top]
			top = top - 1
			pc = pc + 2
		elseif code[pc] == "prt" then
			print("PUG stdout: =========> " .. pt.pt(self.stack[top]))
			top = top - 1
			pc = pc + 1 
		elseif code[pc] == "load" then
			top = top + 1
			self.stack[top] = self.mem[code[pc+1]]
			pc = pc + 2
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
		elseif code[pc] == "jmpIfZ" then
			if self.stack[top] == 0 then
				pc = pc + code[pc+1] + 1
			elseif self.stack[top] == 1 then
				pc = pc + 2
			else 
				error("runTimeError: Invalid code generation - boolean value is neither 0 or 1: " .. self.stack[top])
			end
			top = top - 1
		elseif code[pc] == "jmpIfNZ" then
			if self.stack[top] == 1 then
				pc = pc + code[pc+1] + 1
			elseif self.stack[top] == 0 then
				pc = pc + 2
			else 
				error("runTimeError: Invalid code generation - boolean value is neither 0 or 1: " .. self.stack[top])
			end
			top = top - 1
		elseif code[pc] == "jmp" then
			pc = pc + code[pc+1] + 1
		elseif code[pc] == "arrayalloc" or code[pc] == "larrayalloc" then 
			local array = true
			if code[pc] == "arrayalloc" then
				self.mem[code[pc+1]] = self.stack[top]
				top = top - 1
				array = self.mem[code[pc+1]]
			elseif code[pc] == "larrayalloc" then
				self.stack[base - code[pc+1] + 1] = self.stack[top]
				top = top - 1
				array = self.stack[base - code[pc+1] + 1]
			else
				error("codeGenInternalError: array codeGen is neither arrayalloc nor larrayalloc: " .. code[pc])
			end
			local dim = code[pc+2]
			pc = pc + 3 -- opcode + arraybase + dim
		elseif code[pc] == "arrayset" or code[pc] == "larrayset"then
			local array = true
			if code[pc] == "arrayset" then
				array = self.mem[code[pc+1]]
			elseif code[pc] == "larrayset" then
				array = self.stack[base - code[pc+1] + 1]
			else
				error("codeGenInternalError: array codeGen is neither arrayset nor larrayset: " .. code[pc])
			end
			local dim = code[pc+2]
			local index = -1
			for i = dim,2,-1 do
				index = self.stack[top + 1 - i]
				if type(index) == "table" then
					error("runTimeError: array index is a lua table: " .. pt.pt(index))
				elseif index <= 0 then
					error("runTimeError: array index is less than 1: " .. tostring(index))
				end
				array = array[index]
				if type(array) ~= "table" then
					error("runTimeError: array is not a lua table: " .. type(array))
				end
			end
			index = self.stack[top]
			if type(index) == "table" then
				error("runTimeError: array index is a lua table: " .. pt.pt(index))
			elseif index <= 0 then
				error("runTimeError: array index is less than 1: " .. tostring(index))
			end
			array[index] = self.stack[top - dim]
			top = top - dim - 1 -- 'dim' indices + value being set
			pc = pc + 3 -- opcode + arraybase + dim
		elseif code[pc] == "arrayget" or code[pc] == "larrayget" then
			local array = true 
			if code[pc] == "arrayget" then
				array = self.mem[code[pc+1]]
			elseif code[pc] == "larrayget" then
				array = self.stack[base - code[pc+1] + 1]
			else
				error("codeGenInternalError: array codeGen is neither arrayget nor larrayget: " .. code[pc])
			end
			local dim = code[pc+2]
			local index = -1
			for i = dim,2,-1 do
				index = self.stack[top + 1 - i]
				if index <= 0 then
					error("runTimeError: array index is less than 1: " .. tostring(index))
				end
				if type(index) == "table" then
					error("runTimeError: array index is a lua table: " .. pt.pt(index))
				elseif index <= 0 then
					error("runTimeError: array index is less than 1: " .. tostring(index))
				end
				array = array[index]
				if type(array) ~= "table" then
					error("runTimeError: array is not a lua table: " .. type(array))
				end
			end
			index = self.stack[top]
			if type(index) == "table" then
				error("runTimeError: array index is a lua table: " .. pt.pt(index))
			elseif index <= 0 then
				error("runTimeError: array index is less than 1: " .. tostring(index))
			end
			local rvalue = array[index]
			top = top - dim -- 'dim' indices
			top = top + 1
			self.stack[top] = rvalue
			pc = pc + 3 -- opcode + dim + arraybase

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
