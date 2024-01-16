--- Textual answer outputted by lua :)
--
print("In dynamically typed languages like Python and lua, this is possible.")
print("In statically typed languages C/C++ with array as memory and pointers and integers as distinct types, this is not possible without hack as type checking will fail")
print("a[1] has type X and a has type pointer to type X")
print("A hack in C to fix this is to cast pointer to integer and cast integer back to pointer before applying the dereference operation")
print("Another method would be if the language had array as memory and used the same type for integers and pointers")
