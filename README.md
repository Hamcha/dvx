# DVX
*IL LINGVAGGIO AVTARCHICO*

[Official specs](https://docs.google.com/document/d/1bEthVgMkEh19b75PKIgeyw4iFHJ7ZS6lVh7fNSCW_SY/edit?usp=sharing) (in italian)

## Current Status

#### Parser status

| :ok:                     | Test file   | Notes                               |
|:------------------------:|-------------|-------------------------------------|
| :heavy_exclamation_mark: | hello.dvx   | Multiword string parsed incorrectly |
| :white_check_mark:       | test.dvx    | -                                   |
| :white_check_mark:       | test2.dvx   | -                                   |
| :white_check_mark:       | short.dvx   | -                                   |

#### Interpreter status

| :ok:                     | Test file   | Notes                               |
|:------------------------:|-------------|-------------------------------------|
| :white_check_mark:       | hello.dvx   | -                                   |
| :information_source:     | test.dvx    | Var decls doesn't work, no output   |
| :heavy_exclamation_mark: | test2.dvx   | Func def doesn't work, resolve err  |
| :information_source:     | short.dvx   | Var decls doesn't work, no output   |

## Todo

- Basic interpreter (0.0.3)
- More tests (Recursion, Shadowing, ..)
- Compiler? (LLVM-based, probably)
