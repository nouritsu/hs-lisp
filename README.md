# About
A bare minimum lisp interpreter written in Haskell.

# Installation / Usage
## Using Cabal
Running interpreter in REPL mode
```bash
cabal run
```
Interpreting a file
```bash
cabal run hs-lisp <file>
```

## Using GHC
Compiling
```bash
ghc "./app/Main.hs" [-o <output-name>]
```
Running
```
./<program-name> [file]
```
If file arg is not provided, interpreter will run in REPL mode.

# Features
## Literals
### String Literals
"\[content\]"  
Note that strings are RSR5 compliant meaning they do support escape characters (escaped quotes, backslashes, newlines, carriage returns and tab)  
  
### Number Literals
123, #b123, #x1a4, #d123  
NOTE : CURRENTLY ONLY INTEGERS SUPPORTED  
\# followed by b,x or d can denote binary, hexadecimal or denary.
  
Follows standard lisp syntax.
## Native Functions
### Math
`/`, `*`, `+` and `-` operators have been implemented and work as expected -
```lsp
Lisp :> (+ 5 3)
8
Lisp :> (* 2 8)
16
...
```
`mod`, `quotient` and `remainder` functions work as expected
```lsp
Lisp :> (mod 20 5)
0
Lisp :> (remainder 981 314)   
39
```
Note that `mod` floors operands and returns the remainder after the floor operation. `rem` does the same but turncates the oeprands rather than flooring them.

### Comparison
`=`, `<`, `>`, `<=`, `>=` and `/=` used for comparing number.
```lsp
Lisp :> (/= 0 5)   
#t
Lisp :> (<= 10 9)
#f
```
`string=?`, `string<?`, `string>?`, `string<=?`, `string>=?` and `string/=?` used to compare strings.
```lsp
Lisp :> (string/=? "5" "6")
#t
Lisp :> (string>? "a" "a")  
#f
```
`eq?`, `eqv?` and `equal?`work as expected. `eq?` and `eqv?` are the same.
### Boolean Math
`&&` and `||` AND and OR respectively.  
`not` function can be imported using `load <path-to-included-stdlib-file>`

### List Operations
`cons`, `cdr` and `car` work as expected.  
`apply` can be used to apply a function to a list.

### IO Operations
Other LSP files can be loaded using - `load`  
Ports can be opened using - `open-input-file` and `open-output-file`  
Ports can be closed using - `close-input-port` and `close-output-port`  
Write using - `write`  
Read using - `read`, `read-contents` and `read-all`  

## Standard Library
A small subset of the Lisp STL has been implemented in the file `stdlib.lsp`.  
The library can be loaded using `load <path-to-stdlib`.  
Open the file to find out which functions are defined in it.  

# Footnotes
I've been learning Haskell using this [book](http://learnyouahaskell.com/).  
This project is inspired by this [book](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

