# DVX
*IL LINGVAGGIO AVTARCHICO*

[Official specs](https://docs.google.com/document/d/1bEthVgMkEh19b75PKIgeyw4iFHJ7ZS6lVh7fNSCW_SY/edit?usp=sharing) (in italian)

## Current Status

#### Test status

| :ok: | Test file | Notes |
|:----:|-----------|-------|
| :white_check_mark: | arith.dvx | - |
| :white_check_mark: | cmp.dvx | - |
| :white_check_mark: | hello.dvx | - |
| :white_check_mark: | if.dvx | - |
| :white_check_mark: | mulstr.dvx | - |
| :white_check_mark: | nested.dvx | - |
| :heavy_exclamation_mark: | read.dvx | (doesn't update the variable) |
| :white_check_mark: | recursive.dvx | - |
| :white_check_mark: | short.dvx | - |
| :white_check_mark: | test2.dvx | - |
| :white_check_mark: | test.dvx | - |

#### What doesn't work

- Loops (X VOLTE RIPETO/FINCHE X)
- Reading from stdin (because variables are immutable)

## Todo

- Implement missing statements (FOR/WHILE) (0.0.4)
- Make recursion work (0.0.4/0.0.5)
- More tests (Recursion, Shadowing, ..)
- Implement functions side effects

## Long term goals

- Compiler? (LLVM-based, probably)

## Documentation

[Here it is!](https://hamcha.github.io/dvx/docs/)

Current status (as of last README edit):
```
Haddock coverage:
 100% (  9 /  9) in 'Dvx.Utils'
 100% (  4 /  4) in 'Dvx.Romans'
 100% (  3 /  3) in 'Dvx.Tokens'
  53% (  8 / 15) in 'Dvx.Parser'
 100% ( 11 / 11) in 'Dvx.Interpreter'
  11% (  1 /  9) in 'Std'
 100% (  6 /  6) in 'Main'
```
