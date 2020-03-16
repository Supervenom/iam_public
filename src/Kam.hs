module Kam where

import Parser
import PPrinter

data Closure = Closure { clTrm :: Term
                       , clEnv :: Environment }

instance Show Closure where
    show (Closure trm env) = "(" ++ (show trm) ++ "," ++ (show env) ++ ")"

data Substitution = Substitution { subName :: Name
                                 , subCl :: Closure }

instance Show Substitution where
    show (Substitution var cl) = "[" ++ var ++ "<-" ++ (show cl) ++ "]"

type Environment = [Substitution]

type Stack = [Closure]

data StateK = StateK { trmK :: Term
                     , env :: Environment
                     , stack :: Stack } deriving Show

transK :: StateK -> StateK
transK (StateK (App term1 term2) env stack) = StateK term1 env ((Closure term2 env):stack)
transK (StateK (Abs var term) env (cl:stack)) = StateK term ((Substitution var cl):env) stack
transK (StateK (Var var) env stack) = StateK term env2 stack
    where
      p = findCl var env
      term = clTrm p
      env2 = clEnv p
transK _ = finalK

findCl :: Name -> Environment -> Closure
findCl _ [] = Closure (Var "Final") []
findCl var1 ((Substitution var2 cl):env)
                        |var1 == var2 = cl
                        |otherwise = findCl var1 env

finalK :: StateK
finalK = StateK (Var "Final") [] []


runK :: String -> IO ()
runK s = do
    writeFile "runK.run" ""
    let e = parseExpr s
    case e of
        Left err -> putStr "err"
        Right ex -> appendFile "runK.run" (showTable [ ColDesc center "Term"  left  (show . trmK)
                     , ColDesc center "Environment" left  (show . env)
                     , ColDesc center "Stack"  left (show . stack)
                     ] (runningK [(StateK ex [] [])]))



runningK :: [StateK] -> [StateK]
runningK xs
        | trmK (last xs) == Var "Final" = xs
        | otherwise = runningK (xs ++ [(transK (last xs))])

