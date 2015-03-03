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
| :white_check_mark: | mulstr.dvx | - |
| :white_check_mark: | nested.dvx | - |
| :white_check_mark: | short.dvx | - |
| :white_check_mark: | test.dvx | - |
| :white_check_mark: | test2.dvx | - |

#### What doesn't work

- Ifs (SE/ALLORA/SENNÒ)
- Loops (X VOLTE RIPETO/FINCHE X)
- Recursion

## Todo

- Implement missing statements (IF/FOR/WHILE) (0.0.4)
- Make recursion work (0.0.4/0.0.5)
- More tests (Recursion, Shadowing, ..)

## Long term goals

- Compiler? (LLVM-based, probably)

## Documentation

Just run `sh gendocs.sh`

Current status (as of last README edit):
```
Haddock coverage:
   0% (  0 /  9) in 'Dvx.Utils'
   0% (  0 /  5) in 'Dvx.Romans'
  33% (  1 /  3) in 'Dvx.Tokens'
  47% (  7 / 15) in 'Dvx.Parser'
 100% ( 11 / 11) in 'Dvx.Interpreter'
  11% (  1 /  9) in 'Std'
   0% (  0 /  6) in 'Main'
```
