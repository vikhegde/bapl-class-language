local lpeg = require "lpeg"

function foldA(tbl)
	local acc = tbl[1]
	for i = 2,#tbl,2 do
		if tbl[i] == "+" then
			acc = acc + tbl[i+1]
		elseif tbl[i] == "-" then
			acc = acc - tbl[i+1]
		else
			error("Unknown additive operator")
		end
	end
	return acc
end

function foldM(tbl)
	local acc = tbl[1]
	for i = 2,#tbl,2 do
		if tbl[i] == "*" then
			acc = acc * tbl[i+1]
		elseif tbl[i] == "/" then
			acc = acc / tbl[i+1]
		elseif tbl[i] == "%" then
			acc = acc % tbl[i+1]
		else
			error("Unknown multiplicative operator")
		end
	end
	return acc
end

function foldExp(tbl)
	local acc = tbl[1]
	for i = 2,#tbl,2 do
		if tbl[i] == "^" then
			acc = acc ^ tbl[i+1]
		else
			error("Not eexponentiation operator")
		end
	end
	return acc
end
	
		
local sign = lpeg.S("+-")
local number = lpeg.R("09")^1 / tonumber * lpeg.P(" ")^0
local opA = lpeg.C(lpeg.S("+-")) * lpeg.P(" ")^0
local opM = lpeg.C(lpeg.S("*/%")) * lpeg.P(" ")^0
local opExp = lpeg.C(lpeg.S("^")) * lpeg.P(" ")^0
local termExp = lpeg.Ct(number * (opExp * number) ^ 0) / foldExp
local termMul = lpeg.Ct(termExp * (opM * termExp) ^ 0) / foldM
local expression = lpeg.P(" ")^0 * (lpeg.Ct(termMul * (opA * termMul) ^ 0) / foldA) * -1


print(expression:match(" 4 * 4^3 -3 + 4 / 4 % 3"))
print(expression:match(" 1 % 3"))
