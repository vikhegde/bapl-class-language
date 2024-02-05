# Final Project Report: [The PUG programming language]

## Language Syntax

In this section, describe the overall syntax of your language.
Here is a brief description of the syntax of the PUG language (named after my favorite dog breed)
1. line comments are # and multiline comments are #{ and #}
2. A program consists of a list of function prototypes and function definitions.
3. Neither function prototypes nor function definitions need a terminating semi-colon after the function block
4. Multiple function prototypes both before and after the function definition are allowed provided the return type
   and the number and types of parameters match
5. A function can be as simple as an empty block (a pair of brackets) or a block with a single empty statement
   i.e. {;}
6. A function 0, 1 or more parameters. The last paramter can have a default value which is used if a function call
   does not use the last argument
7. There must be a return type. Return types can be void or a "num". Currently an array cannot be returned from
   a function so array types are not a valid return type
8. array types also cannot be function parameters or arguments
9. The main function is required. Program execution starts with the main function
10. The main function does not take any arguments.
11. The main function only returns a num type. main always returns a number to lua (the parent environment). main()
    cannot return type void.
12. Four types are supported by the PUG language. They are "num" (a numeric type. No distinction is made  between
    floating point numbers and integers.), void (a type that implies no type), "unknownType" (a type that is yet to
    be deduced"), array. Note that an array of one dimension is of a different type than an array of two dimensions.
    So in effect there are an infinite number of types in PUG.
13. multidimensional arrays of any number of dimensions are supported.
14. Just like functions, variables need to be declared with a type before use.
15. local variables override function parameters with the same name and function parameters override globals with the
    same name.
16. local variables are transient and exist only within the block they are declared and any (further inner blocks).
    They are not visible in an outer block. For the purposes of local variables a function block behaves similar to a
    nested or inner block. A local variable declared inside a function block is not visible in another function
17. Global variables too need to be declared. But once declared they are visible in all functions and all nested
    blocks unless they are hidden by function parameters or local variables with the same name.
18. function parameters are similar to global variables except that they are typed by definition (since function
    declarations reuire types.). function parameters can be assigned to and used like any other variable
    but are lost when the function is exited.
19. A function defintion consists of a return type, zero, one or more function parameters with types and optionally
    a last parameter with a default value. Function invocations can choose to skip providing the final argument
    (if a default argument is specified in the function defintion) and in this case the default value will be
    transparently passed to the function call. Finally a function defintion has body
20. The function body can be empty, it can have an empty statement (which is just the semi-clon) or one or
    more statements.
21. Every statement must be terminated by a semi-colon and this includes statements that contain blocks like
    if-elseif-else or while or do-while. Even nested blocks must be terminated by a semi-colon
22. A statement can be an assignment statement, void statement (which is a special statement where an expression
    is cast to a void), print statement (@) or a return statement.
23. expressions are rvalues i.e. somethiong that has a value. An expression by itself terminated by a semi-colon
    is not allowed unless it is cast to a void.
24. An lvalue is not allowed in a statement without an assignment statement and an rvalue on the right hand
    side of the equals operator.
25. An rvalue has a value and a type. An lvalue also has a type but has no value but instead points to
    assignable space either on the stack or in memory.
26. An rvalue can be unary like a number literal or an array or it can be the result of an arithematic
    binary operation or a relational operation  or a logical operation
27. Arithematical operations include +, -, *, / % and ^ (the power operator)
28. relational operations are <=, >=, <, >, ==, !=
29. Logical operations are && and ||
30. There is a print statement represented by the @<value>. @ can handle both numbers and multidimensional
    arrays. Multidimensional arrays are printed using the provided pt module.
31. There is a return statement that both returns control to the caller as well as returns a type
    and a value.
32. void functions can use return but they cannot provide a return value in the return statement.
33. A function returning num may not have a return statement in which case 0 is returned to the caller.
34. There is a new expression  which allocates a (multi) dimensional array. A new expression can only be
    the expression in the LHS of an assignment statement. This is to ensure that an allocated array is
    not discarded or operated upon with other operators. 
35. arrays can be allocated to all three types of variables - global, local and function parameters.
36. arrays can be printed.
37. arrays cannot be returned from functions, passed to functions or be function parameters.
38. [Basic expectation]: ternary operators are supported. They work in the normal C function with short-circuit
    evaluation. In (a > b) ? exp1 : exp2, both exp1 and exp2 must be of same type and also non-void type. 
39. [basic expectation]: unless statement is supported. unless (exp) is true the unless-block is executed.
40. [challenge expectation]: a type system as outlined in previous lines is implemented.

## New Features/Changes

The following new feature has been added:
* A type system for the PUG programming language: A type system has been implemented and I believe fulfils all the
  guidelines given by the instructors for implementing a type system as a couse project.
* Type system details:  The type system supports only four types: unknownType (indicates the type is unknown and yet to
  to be resolved), void type(indicates absence of a type), num (for number), num[][][][].... (for arrays -
  multidimensional array of any number of dimensions is supported. A single dimensional array is not the same type as a
  two dimensional array. For example:  num[] != num[][] in PUG syntax). All variables have to be assigned a type at
  first declaration and they carry the type around for their lifetime. A limited form of casting is provided where a
  nonvoid type can be cast to void indicating that a value is being discarded. An rvalue as a statement must cast its
  value to void since a statement has void type. Return values of functions are checked against the return type of the
  function definition to ensure they match. Similarly number of  function parameters and their types must match
  function call arguments. All expressions have types. A block has void type. A statement has void type. A final type of
  unknowntype (normal if it happens during compilation bit abnormal if it occurs after compilation) is an error in the
  language implementation. All assignment operations as well as binary operations are checked to ensure type of
  LHS == type of RHS.
* Example usage and sample programs: Here are some sample programs and a rationale for why it is a sample program:
* The sample PUG programs are in the samples subdirectory of the course-project directory in the github repository.
1. stress.pug - this is best used as the first test while testing the implementation at it exercises many features
   of the language.
2. comments.pug - A main function and comments. The main function returns 1. Comments are ignored.
3. simplest.pug - a main() function with an empty block. Empty blocks are legal in PUG.
4. no-main-params.pug - a program that fails because main() cannot take parameters.
5. no-void-main-return.pug - a program that fails because return type of main() cannot be of void type.
6. no-non-num-main-return.pug - a program that fails because return type of main() cannot be of void type.
7. no-missing-main.pug - a program that fails main function is missing.
8. empty-statement.pug - PUG language can have an empty statement (single semicolon).
9. no-main-return-mismatch.pug - a program that fails because main has a return statement of type that is not num.
10. no-non-main-return-mismatch.pug - a program that fails because a non-main function has a return statement that
    does not match its return type.
11. no-cast-to-void-for-statement.pug - a program that fails because a statement (which is not an expression) is
    being cast to void.
12. missing-cast-to-void-for-expression-statement.pug - a program that fails because an expression statement is not
    cast to void.
13. cast-to-void-for-expression-statement.pug - the previous program that now works because the expression is cast
    to void.
14. no-multiple-function-definitions.pug - a function cannot be redefined.
15. compatible-function-prototypes.pug - a function definition can have 1, 2 or more function prototypes both before
    and after the function defintion.
16. no-incompatible-function-prototypes-params.pug - a function definition must match all function prototype(s) params.
17. no-incompatible-function-prototypes-retType.pug - a function definition must match all function prototype(s)
    return types. 
18. no-semicolon-for-function-prototypes.pug - a function prototype does not require a terminating semi-colon.
    This is a feature and not a bug :-)
19. no-assign-type-mismatch.pug - a program that fails because LHS and RHS of assignment don't match.
20. no-add-type-mismatch.pug - a program that fails because a sample arithematic binary operator has LHS/RHS mismatch.
21. no-relational-type-mismatch.pug - a program that fails because a sample relational binary operator has LHS/RHS
    mismatch.
22. no-logical-type-mismatch.pug - This is actually not desirable but I dont have the time to fix this. Ideally I will
    have a bool types to which each operand of a logical operator evaluates to.
23. non-function-block-needs-semicolon - This program fails because an inner i.e. non-function block requires a
    semi-colon. This is a wart but I dont have time to fix it.
24. non-function-block-needs-semicolon-take-2 - same program as previous one except that it succeeds because it has
    semi-colon after inner block.
25. no-new-outside-assignment-statement - a program that fails because new is used other than as a LHS of a assignment
    statement.
26. power-operator.pug - Some fancy computations using the power operator.
27. print-num.pug - print statement printing the result of a factorial function. this also shows recursive function
    calls and default arguments.
28. print-array.pug - print statement printing an array in lua table form display.
29. statement-terminator-needed.pug - every statement in PUG needs a semicolon including if/while statements.
    This is different from C where they are optional. Conversely every function (including function prototypes) do not
    need a semi colon after the block. Here is an example of almost valid while statement without terminating
    semi-colon that causes compile error
30. statement-terminator-needed-take-2.pug - same as previous but works because while block has terminating semi-colon.
31. locals-valid-only-in-current-scope.pug - a local declared inside an inner scope is not visible outside the block.
32. locals-in-enclosing-scope.pug - a local declared inside an enclosing scope is visible inside the block.
33. locals-in-nearest-scope.pug - a local declared inside an nner scope overrides a local declared in an outer scope.
34. locals-override-function-params.pug - locals override function parameters.
35. locals-override-globals.pug - locals override global variables.
36. parameters-override-globals.pug - function parameters override global variables.
37. parameters-are-tmp-lvalues.pug - parameters are assigned to but they are not copied back to caller environment.
38. ternary-operator.pug - shows both outcomes of a ternary operator
39. unless-statement.pug - shows both outcomes of unless operator
40. if-statement.pug - shows an if-statement
41. if-else-statement.pug - shows an if-else-statement
42. if-elseif-else-statement.pug - shows an if-else-elseif statement
43. while-statement.pug - shows a while statement
44. do-while-statement.pug - shows a do-while loop with unconditional execution of first loop
45. do-while-statement-take-2.pug - shows a do-while loop with multiple loops
46. [wart]: boolean-expression-must-be-result-of-relational.pug - shows that program fails if boolean expression is
    not result of relational expression. This is a wart because it is a runtime error rather than a compile time error.
    Note that my type system does not implement a boolean type.
    

* Tradeoffs and limitations:
1. arrays cannot be passed as parameters to functions or be returned from functions.
   Supporting this requires more work and I ran out of time
2. There are no bool, string, structs types.
3. There are no varargs
4. There is no garbage collection
5. There is no bounds checking in arrays
6. Implementation is not optimized at all
7. There are no tests for the implementation other than the sample programs.
8. The implementation could be cleaner and more organized.
9. error reporting is very primitive. syntax error locations are not very accurate

## Future

In this section, discuss the future of your language / DSL, such as deployability (if applicable), features, etc.

* What would be needed to get this project ready for production?
1. The design of the error reporting needs to be improved. This is the single biggest weakness in this project.
   Currently it does not report exactly where a syntax error may have happened or what the syntax error is.
   It just points at the last position at which the parse failed which in many syntax errors does not happen to
   be exactly where the error happened.
   Even with my familiarity with the internals I am often confounded when the compiler indicates a syntax
   error with absolutely no clue about what the error is. I would hate to impose this on a naive user.
2. The number of types should be expanded to at least include bool, strings as well as structs.
3. The type system passes types as strings. It would be cleaner to use some sort of numeric/enumerated integer lua 
   value to indicate types.
   
* How would you extend this project to do something more? Are there other features you’d like? How would you go about adding them?
1. I would like as a first step convert the interpreted implementation into a transpiler where the code generator
   emits C code and we can compile the C code with any c compiler to get a x86 machine code binary.
   The transpiler it seems will be relatively straight forward. I have a rudimentary type system that can
   map to C types and I have while loops, if-then-else and blocks and functions - most of which map directly to C. 
2. Once that is working correctly, I would like to implement 8080 (it is an old architecture but simpler than 8086)
   native assembly generation as well as an assembler.
   As a hobby I am working on a 8080 simulator (and I have access to public 8080 ROMs to verify correctness)
   and this would help both ways - use the simulator to test the compiler/assembler and use the compiler/assembler
   to test the simulator.
3. I would also like to implement garbage collection
4. varargs are also something I can implement to complete the feature set.
5. Finally I would also like to try my hand at simple optimizations. I am very new to this area and it is not clear
   what optimizations I would implement. Optimization is a large field and area of study and I would probably
   implement some simple ones.

## Self assessment

* Self assessment of your project: for each criteria described on the final project specs, choose a score (1, 2, 3) and explain your reason for the score in 1-2 sentences.
1. Language Completeness - self assessment 3 - implemented both 2 basic features (unless statement and ternary operator)
   as well as a challenge feature (type system)
2. Code Quality and Report - self-assesment 2 - plus points: working code, comprehensive report, comprehensive
   documentation and comprehensive sample programs. minus points - sub-par error reporting. While syntax error
   shows approximate location of error, no message says what exactly is wrong.
3. Originality and Scope - self-assesment 2.5 - while what I have implemented cannot probably be classified
   as a minor extension in my honest opinion it is not original enough or large enough to deserve a 3 - 
   so I think this is somewhere in-between so 2.5 points

* Have you gone beyond the base requirements? How so?
  I have attempted to go beyond the base requirement by implementing a type system which was suggested by the
  instructors as going beyond the base requirements. Whether I have really achieved this is for the instructors
  to decide. I am sure I will learn a lot from the feedback given by the instructors no matter what the score.

## References
1. I mostly referred to the course materials such as: The LUA programming language book, the LPEG primer/tutorial
   and occassionally stack-overflow website to figure out LUA syntax (for example, I used stack-overflow very early
   in the class to find out that not-equal-to in LUA is '~=' and not '!=')
2. I also found the pt module supplied by the instructors and used it extensively in both debugging and in my 
   homeworks and project.
   

