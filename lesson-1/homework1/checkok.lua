#!/usr/bin/env lua

function checkok(result, target)
	if result == target then
		print("ok")
	else
		print("result "..(result or "nil").." does not match target "..(target or "nil"))
	end
end
