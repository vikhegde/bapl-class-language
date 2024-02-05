# Final Project Report: [The PUG programming language]

## Language Syntax

In this section, describe the overall syntax of your language.

## New Features/Changes

The following new feature has been added:
* A type system for the PUG programming language
* Type system details
* Example usage and sample programs
* Tradeoffs and limitations: arrays cannot be passed as parameters to functions or be returned from functions

## Future

In this section, discuss the future of your language / DSL, such as deployability (if applicable), features, etc.

* What would be needed to get this project ready for production?: The design of the error reporing needs to be improved. Currently it does not report exactly where a syntax error may have happened. It just points at the last at which the parse failed which in many syntax errors does not happen to be where exactly the error happened. Also the number of types should be exanded to at least include bool, characters and strings as well as structs and lists.
* How would you extend this project to do something more? Are there other features you’d like? How would you go about adding them? : I would like to make this into a transpiler where the code generator emits C code and we can compile the C code to get a x86 machine code binary. I would also like to try my hand at simple optimizations

## Self assessment

* Self assessment of your project: for each criteria described on the final project specs, choose a score (1, 2, 3) and explain your reason for the score in 1-2 sentences.
* Have you gone beyond the base requirements? How so?

## References

List any references used in the development of your language besides this courses, including any books, papers, or online resources.
