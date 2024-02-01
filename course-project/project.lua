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
local opAdd = lpeg.S"+-" * whitespace

-- Pattern for multiplicative operators
local opMul = lpeg.S"*/%" * whitespace

-- Pattern for exponential operator
local opExpo = lpeg.P"^" * whitespace

-- Pattern for unary minus and plus
local opSign = opAdd

-- Pattern for relational operators - order is important
local opRel = (lpeg.P("<=") + lpeg.P(">=") + lpeg.P("<") + lpeg.P(">") + lpeg.P("==") + lpeg.P("!=")) * whitespace

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

-- Patterns for comma (funcDef and funcCall)
local comma = lpeg.P(",") * whitespace

-- List of keywords
local keywords = { "and", "or", "if", "elseif", "else", "while", "do", "function"}
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

Compiler = {grammar = {}, ast = {}, code = {}}
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
			powerExp = lpeg.Ct(((ID / Compiler:astNode("var", "name")
				+ number / Compiler:astNode("numericLiteral", "value"))
				* ((lpeg.C(opExpo) 
				* (ID / Compiler:astNode("var", "name")
					+ integer / Compiler:astNode("numericLiteral", "value")))^0)))
						/ foldBin, 
			unaryExp = lpeg.Ct(((lpeg.C(opSign)) ^ 0) * powerExp)
						/ foldUnary,
			multExp = lpeg.Ct((unaryExp * ((lpeg.C(opMul) * unaryExp)^0)))
					/ foldBin, 
			addExp = lpeg.Ct((multExp * ((lpeg.C(opAdd) * multExp)^0)))
					/ foldBin,
			relExp = lpeg.Ct(addExp * ((lpeg.C(opRel) * addExp)^-1))
					/ oneBinaryOp,
			rvalue = lpeg.Ct((relExp * ((lpeg.C(opLogical) * relExp)^0)))
					/ foldBin,
			lvalue = ID/Compiler:astNode("var", "name"),
                        -- lvalue and rvalue all by themselves are valid statements
			ifelseifstat = 
			                OP 
					* rvalue
					* CP
					* block
					* KEYWORD("elseif") * (ifelseifstat + ifelsestat + ifstat) -- order
					/ Compiler:astNode("if2", "predExp", "ifblock", "elseifstat"),
			ifelsestat = 
			                OP 
					* rvalue
					* CP
					* block
					* elsestat
					/ Compiler:astNode("if2", "predExp", "ifblock", "elsestat"),
			ifstat = 
			                OP 
					* rvalue
					* CP
					* block
					/ Compiler:astNode("if2", "predExp", "ifblock"),
			elsestat = KEYWORD("else") 
					* block
					/ Compiler:astNode("else2", "elseblock"),
                        stat = ((lvalue * opAssign * rvalue)
					/ Compiler:astNode("assign", "lvalue", "rvalue")
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
				+ lvalue
			        + rvalue),
			stats = stat * ((separator^1) * stat) ^ 0,  -- multiple separators ;;;; are legal
                        -- ; after block is legal
                        -- empty block is legal
			block = OB * lpeg.Ct(stats^0) * CB * (separator^0) / Compiler:astNode("block", "stats"),
			funcDef = (KEYWORD("function")
					* ID 
					* OP * lpeg.Ct((ID * (comma * ID)^0)^0) * CP
					* block
					) / Compiler:astNode("funcDef", "funcName", "arglist", "body")
		}

Compiler.grammar = grammar * -1

function Compiler:parse(text)
	self.ast = self.grammar:match(text)
	-- tricky - cannot use 'not' here since ast may be a boolean value
	if self.ast == nil then
		print("Syntax Error: Failed to parse pug program")
	end
end
local input = io.read("*a")
Compiler:parse(input)
print(pt.pt(Compiler.ast))
