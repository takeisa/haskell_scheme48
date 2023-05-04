# Scheme implementation on Haskell

Implemented with reference to [Write Yourself a Scheme in 48 Hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

- Project management by stack
- The functions were divided into multiple modules.

## How to execute
```sh
$ stack run -- '(- (+ 1 2 (* 4 5) (/ 4 2)) 10)'
15
```

It can only do four arithmetic operations yet.
