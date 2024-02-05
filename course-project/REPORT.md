# Final Project Report: [The PUG programming language]

## Language Syntax

In this section, describe the overall syntax of your language.

## New Features/Changes

The following new feature has been added:
* A type system for the PUG programming language: A type system has been implemented and I believe fulfils all the guidelines given by the instructors for implementing a type system as a couse project.
* Type system details:  The type system supports only four types: unknownType (indicates the type is unknown), void type(indicates absence of a type), num (for number), num[][][][].... (for arrays - multidimensional array of any number of dimensions is supported. A single dimensional array is not the same type as a two dimensional arra. For example:  num[] != num[][] in PUG syntax). All variables have to be assigned a type at first declaration and they carry the type around for their lifetime. A limited form of casting is provided where a nonvoid type can be cast to void indicating that a value is being discarded. An rvalue as a statement must cast its value to void since a statement has void type. Return values of functions are checked against the return type of the function definition to ensure they match. Similarly number of  function parameters and their types must match function call arguments. All expressions have types. A block has void type. A statement has void type. A type of unknowntype (if it occursi) is an error in the language implementation. All assignment operations as well as binary operations are checked to ensure  type of LHS == type of RHS.
* Example usage and sample programs: Here are some sample programs and a rationale for why it is a sample program:
0. The sample PUG programs are in the samples subdirectory
1. simplest.pug - a main() function with an empty block. Empty blocks are legal in PUG.
2. no-main-params.pug - a program that fails because main() cannot take parameters
3. no-void-main-return.pug - a program that fails because return type of main() cannot be of void type
4. no-non-num-main-return.pug - a program that fails because return type of main() cannot be of void type
5. no-missing-main.pug - a program that fails main function is missing
6. empty-statement.pug - PUG language can have an empty statement (single semicolon)
7. no-main-return-mismatch.pug - a program that fails because main has a return statement of type that is not num
8. no-non-main-return-mismatch.pug - a program that fails because a non-main function has a return statement that does not match its return type
9. no-cast-to-void-for-statement.pug - a program that fails because a statement (which is not an expression) is being cast to void
10. missing-cast-to-void-for-expression-statement.pug - a program that fails because an expression statement is not cast to void
11. cast-to-void-for-expression-statement.pug - the previous program that now works because the expression is cast to void
12. no-multiple-function-definitions.pug - a function cannot be redefined
13. compatible-function-prototypes.pug - a function definition can have 1, 2 or more function prototypes both before and after the function defintion
14. no-incompatible-function-prototypes-params.pug - a function definition must match all function prototype(s) params
15. no-incompatible-function-prototypes-retType.pug - a function definition must match all function prototype(s) return types 
16. no-semicolon-for-function-prototypes.pug - a function prototype does not require a terminating semi-colon. This is a feature and not a bug :-)
17. no-assign-type-mismatch.pug - a program that fails because LHS and RHS of assignment don't match
18. no-add-type-mismatch.pug - a program that fails because a sample arithematic binary operator has LHS/RHS mismatch
19. no-relational-type-mismatch.pug - a program that fails because a sample relational binary operator has LHS/RHS mismatch
20. no-logical-type-mismatch.pug - This is actually not desirable but I dont have the time to fix this. Ideally I will have a bool types to which each operand of a logical operator evaluates to.
21. non-function-block-needs-semicolon - This program fails because an inner i.e. non-function block requires a semi-colon. This is a wart but I dont have time to fix it.
14
21. non-function-block-needs-semicolon-take-2 - same program as previous one except that it succeeds because it has semi-colon after inner block
22. no-new-outside-assignment-statement - a program that fails because new is used other than as a LHS of a assignment statement
23. power-operator.pug - Some fancy computation using the power operator
24. print-num.pug - print statement printing the result of a factorial function. this also shows recursive function calls and default arguments.
25. print-array.pug - print statement printing an array in lua table form display
26. statement-terminator-needed.pug - every statement in PUG needs a semicolon including if/while statements. This is different from C where they are optional. Conversely every function (including function prototypes) do not need a semi colon after the block. Here is an example of almost valid while statement without terminating semi-colon that causes compile error
27. statement-terminator-needed-take-2.pug - same as previous but runs because while block has terminating semi-colon
28. locals-valid-only-in-current-scope.pug - a local declared inside an inner scope is not visible outside the block
29. locals-in-enclosing-scope.pug - a local declared inside an enclosing scope is visible inside the block
29. locals-in-nearest-scope.pug - a local declared inside an nner scope overrides a local declared in an outter scope
30. locals-override-function-params.pug - locals override function parameters
31. locals-override-globals.pug - locals override global variables
32. parameters-override-globals.pug - function parameters override global variables
33. parameters-are-tmp-lvalues.pug - parameters are assigned to but they are not copied back to caller environment
34. stress.pug - a single program that stresses the PUG language implementation as much as possible

* Tradeoffs and limitations: arrays cannot be passed as parameters to functions or be returned from functions. Supporting this requires more work and I ran out of time

## Future

In this section, discuss the future of your language / DSL, such as deployability (if applicable), features, etc.

* What would be needed to get this project ready for production?: The design of the error reporing needs to be improved. Currently it does not report exactly where a syntax error may have happened. It just points at the last position at which the parse failed which in many syntax errors does not happen to be exactly where the error happened. Also the number of types should be exanded to at least include bool, characters and strings as well as structs and lists. Secondly, the type system passes types as strings. It would be cleaner to use some sort of numeric type to indicate types.
* How would you extend this project to do something more? Are there other features you’d like? How would you go about adding them? : I would like to make this into a transpiler where the code generator emits C code and we can compile the C code to get a x86 machine code binary. I would also like to try my hand at simple optimizationsi. The transpiler it seems will be relatively straight forward. I have a rudimentary type system that can map to C types and I have while loops, if-then-else and blocks and functions - most of which map directly to C. For optimizations, it is not clear what optimizations I would implement. It is a large field and area of study and I would probably implment some simple ones.

## Self assessment

* Self assessment of your project: for each criteria described on the final project specs, choose a score (1, 2, 3) and explain your reason for the score in 1-2 sentences.
* Have you gone beyond the base requirements? How so?

## References
 I mostly refererred to the course materials such as: The LUA programming language book, the LPEG primer/tutorial and occassionally stack-overflow website to figure out LUA syntax (for example, I used stack-overflow very early in the class to find out that not-equal-to in LUA is '~=' and not '!=')
