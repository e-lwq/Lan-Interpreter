# Lan Interpreter
Interpreter & REPL written in Haskell for custom programming language _Lan_, which its syntax is described in `syntax.txt`, along with some built-in functions.

## How to use Lan REPL
After executing Lan.hs, run `lan` to enter Lan REPL.

<ins>REPL commands</ins>
1. `#cd "<folderpath>"`: change directory to `<folderpath>`.
2. `#run "<filepath>"`: execute Lan source code in `<filepath>`.
3. `#format "<filepath>"`: print standard formatted Lan code given in `<filepath>`.
4. `<LanExpr>`: print evaluated Lan expression given in input expression.
5. `#prev` / `#p`: run previous Lan REPL command again.
7. `#quit` / `#q`: exit Lan REPL.

## File contents
1. **Lan.hs**<br>
   Main driver program for loading source code, parsing code into Abstract Syntax Tree, then executing the code in Haskell.
2. **LanDef.hs**<br>
   Type declarations and instance declarations for Lan data types, Lan error types, syntax tree of Lan code, and monads for computation of Lan code.
3. **Parser.hs**<br>
   Code for parsing text file containing Lan code into a syntax tree. (Syntax of Lan code is described in `syntax.txt`.)
4. **Evaluator.hs**<br>
   Code for evaluating Lan expressions and Lan code ie. executing Lan code described by syntax tree in Haskell using do-sequencing.
5. **PrimitiveFuncs.hs**<br>
   Definitions of primitive functions available in Lan, also listed out in `syntax.txt`.
6. **PrimitiveOps.hs**<br>
   Definitions of primitive operations available in Lan, also listed out in `syntax.txt`.
7. **Repl.hs**<br>
   Code for the parser for Lan REPL command lines and executing the commands.
8. **Helper.hs**<br>
   Helper functions used by above files.
9. **syntax.txt**<br>
   Description of Lan syntax, primitive functions and primitive operators.
10. **examplePrograms**<br>
   Holds files containing example programs written in Lan, which can be executed by `#run "examplePrograms/<filename>"`.

   
