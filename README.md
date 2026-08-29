# Lan Interpreter
Interpreter & REPL written in Haskell for custom programming language _Lan_, which its syntax is described in `syntax.txt`, along with some built-in functions.

## How to use Lan REPL
After executing Lan.hs, run `lan` to enter Lan REPL.
<u>REPL commands</u>
1. `#cd <folderpath>`
   Change directory to "`<folderpath>`".
2. `#run <filepath>`
   Execute Lan source code in "`<filepath>`".
3. `#format <filepath>`
   Print standard formatted Lan code given in "`<filepath>`".
4. `<LanExpr>`
   Print evaluated Lan expression given in input expression.
5. `#prev` / `#p`
   Run previous Lan REPL command again.
7. `#quit` / `#q`
   Exit Lan REPL.
