Prerequisites: Haskell and stack installed, see
https://docs.haskellstack.org/en/stable/README/.

To use this tool, inside the tool directory, type:

1. stack setup
2. stack build
3. stack ghci

This way, you are in the interactive shell.

Two functions are exposed, run and runn.

run :: String -> Int -> IO ()

It takes as inputs a lambda-term as a string and an integer, representing the
number of • . It outputs the run in a file named run.run .

runn :: String -> Int -> Int -> IO ()

It takes as inputs a lambda-term as a string, an integer, representing the
number of • , and another integer representing the length of the trace to be
computed. It outputs the run in a file named run.run . This functions is needed
when one wants to see the first n steps of a divergent computation.


Lambda-term syntax:

tu   -> (t)(u)
λx.t -> \\x.t
x    -> x


Example: (λx.xx)(λy.y) -> (\\x.(x)(x))(\\y.y)

Command example: run "(\\x.(x)(x))(\\y.y)" 0
 



