# 🐑
A very shitty lambda calculus, extended with new features.

- Literally just untyped lambda calculus.
- No named parameters, only De Bruijn indices written as multiple dots.
- You can write ints, but they're just syntax sugar for Church numerals.
- The evaluator is really bad right now and will die on anything recursive. (To be fixed later.)
- Basic subtraction can take, like, between seconds and minutes to run.

## Syntax
- Lambda: `\`
- Access closest variable: `.`, second closest: `..`
- Numerals: `0`, `1`, `2`, ...
- Call `a` with `b` as an argument: `a b`
- Left associative: `a b c` is `(a b) c`
- Create a global variable: `name: value`

So, the identity function $\lambda x. x$ is `\.`.
The Church encoding of the number two — $\lambda f. \lambda x. f (f x)$ — is written as `\\.. (.. .)`.

## Stdlib
I've implemented a couple of basic definitions:

## Running
```sh
opam switch create .
dune exec my-lamb
