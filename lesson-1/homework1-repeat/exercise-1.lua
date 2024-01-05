local lpeg = require "lpeg"

pattern = lpeg.P("hello")
print(lpeg.match(pattern, "hello wrold"))
print(lpeg.match(pattern, "hi wrold"))
