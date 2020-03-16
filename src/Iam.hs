module Iam where

import Parser
import PPrinter

data Polarity = Up | Down

instance Show Polarity where
    show Up = "Up"
    show Down = "Down"

instance Eq Polarity where
    Up == Up = True
    Down == Down = True
    _ == _ = False

data Context = Hole | CtxAbs Name Context | CtxAppL Context Term
                | CtxAppR Term Context | CtxEsOut Context Name Term
                | CtxEsIn Term Name Context deriving Eq

instance Show Context where
    show Hole = "□"
    show (CtxAbs var ctx) = "λ" ++ var ++ "." ++ (show ctx)
    show (CtxAppL x y) = "(" ++ (show x) ++ ")" ++ "(" ++ (show y) ++ ")"
    show (CtxAppR x y) = "(" ++ (show x) ++ ")" ++ "(" ++ (show y) ++ ")"
    show (CtxEsOut x y z) = (show x) ++ "[" ++ y ++ "←" ++ (show z) ++ "]"
    show (CtxEsIn x y z) = (show x) ++ "[" ++ y ++ "←" ++ (show z) ++ "]"

data Signature = Signature { sigName :: Name
                           , sigCtx :: Context
                           , sigSig :: [Signature] } deriving Eq

instance Show Signature where
    show (Signature var ctx sig) = "(" ++ var ++ "," ++ (show ctx) ++ "," ++ (show sig) ++ ")"

eSigName :: ExtSig -> Name
eSigName (Sig (Signature var _ _)) = var

eSigCtx :: ExtSig -> Context
eSigCtx (Sig (Signature _ ctx _)) = ctx

eSigSig :: ExtSig -> [Signature]
eSigSig (Sig (Signature _ _ sig)) = sig

data ExtSig = Sig Signature | Pi deriving Eq

instance Show ExtSig where
    show Pi = "•"
    show (Sig s) = show s

signature :: ExtSig -> Signature
signature (Sig s) = s

type IOStack = [ExtSig]

type SigStack = [Signature]

data Lambda = Lambda Term Context SigStack deriving Eq

data State = State { trm :: Term
                   , ctx  :: Context
                   , iost :: IOStack
                   , sst  :: SigStack
                   , pol  :: Polarity } deriving (Show, Eq)

data Trip = Trip Context Context Polarity

ctxLvl :: Context -> Int
ctxLvl Hole = 0
ctxLvl (CtxAbs _ ctx) = ctxLvl ctx
ctxLvl (CtxAppL ctx _) = ctxLvl ctx
ctxLvl (CtxAppR _ ctx) = 1 + (ctxLvl ctx)
ctxLvl (CtxEsOut ctx _ _) = ctxLvl ctx
ctxLvl (CtxEsIn _ _ ctx) = 1 + (ctxLvl ctx)

fstCtx :: Trip -> Context
fstCtx (Trip c _ _) = c

sndCtx :: Trip -> Context
sndCtx (Trip _ c _) = c

polarity :: Trip -> Polarity
polarity (Trip _ _ p) = p

bindCtx :: Trip -> Name -> Trip
bindCtx (Trip Hole ctx Down) var = Trip Hole ctx Down
bindCtx (Trip c@(CtxAppL ctx1 term) ctx2 Down) var
        |p == Up = Trip ctx3 ctx4 p
        |otherwise = Trip c ctx2 p
    where
      r = bindCtx (Trip ctx1 (ctxSubCtx ctx2 (CtxAppL Hole term)) Down) var
      ctx3 = fstCtx r
      ctx4 = sndCtx r
      p = polarity r
bindCtx (Trip c@(CtxAppR term ctx1) ctx2 Down) var
        |p == Up = Trip ctx3 ctx4 p
        |otherwise = Trip c ctx2 p
    where
      r = bindCtx (Trip ctx1 (ctxSubCtx ctx2 (CtxAppR term Hole)) Down) var
      ctx3 = fstCtx r
      ctx4 = sndCtx r
      p = polarity r
bindCtx (Trip c@(CtxAbs var1 ctx1) ctx2 Down) var2
        |p == Up = Trip ctx3 ctx4 p
        |var1 == var2 = Trip c ctx2 Up
        |otherwise = Trip c ctx2 p
    where
      r = bindCtx (Trip ctx1 (ctxSubCtx ctx2 (CtxAbs var1 Hole)) Down) var2
      ctx3 = fstCtx r
      ctx4 = sndCtx r
      p = polarity r
bindCtx (Trip c@(CtxEsOut ctx1 var1 term) ctx2 Down) var2
        |p == Up = Trip ctx3 ctx4 p
        |var1 == var2 = Trip c ctx2 Up
        |otherwise = Trip c ctx2 p
    where
      r = bindCtx (Trip ctx1 (ctxSubCtx ctx2 (CtxEsOut Hole var1 term)) Down) var2
      ctx3 = fstCtx r
      ctx4 = sndCtx r
      p = polarity r
bindCtx (Trip c@(CtxEsIn term var2 ctx1) ctx2 Down) var
        |p == Up = Trip ctx3 ctx4 p
        |otherwise = Trip c ctx2 p
    where
      r = bindCtx (Trip ctx1 (ctxSubCtx ctx2 (CtxEsIn term var2 Hole)) Down) var
      ctx3 = fstCtx r
      ctx4 = sndCtx r
      p = polarity r

ctxSub :: Context -> Term -> Term
ctxSub Hole term = term
ctxSub (CtxAbs var ctx) term = Abs var (ctxSub ctx term)
ctxSub (CtxAppL ctx term1) term2 = App (ctxSub ctx term2) term1
ctxSub (CtxAppR term1 ctx) term2 = App term1 (ctxSub ctx term2)
ctxSub (CtxEsOut ctx name term1) term2 = Es (ctxSub ctx term2) name term1
ctxSub (CtxEsIn term1 name ctx) term2 = Es term1 name (ctxSub ctx term2)

ctxSubCtx :: Context -> Context -> Context
ctxSubCtx Hole ctx = ctx
ctxSubCtx (CtxAbs var ctx1) ctx2 = CtxAbs var (ctxSubCtx ctx1 ctx2)
ctxSubCtx (CtxAppL ctx1 term) ctx2 = CtxAppL (ctxSubCtx ctx1 ctx2) term
ctxSubCtx (CtxAppR term ctx1) ctx2 = CtxAppR term (ctxSubCtx ctx1 ctx2)
ctxSubCtx (CtxEsOut ctx1 var trm) ctx2 = CtxEsOut (ctxSubCtx ctx1 ctx2) var trm
ctxSubCtx (CtxEsIn term var ctx1) ctx2 = CtxEsIn term var (ctxSubCtx ctx1 ctx2)

ctxHole :: (Context, Context) -> (Context, Context)
ctxHole (Hole, Hole) = (Hole, Hole)
ctxHole (ctx, c@(CtxAppL Hole term)) = (ctx, c)
ctxHole (ctx, c@(CtxAppR term Hole)) = (ctx, c)
ctxHole (ctx, c@(CtxAbs var Hole)) = (ctx, c)
ctxHole (ctx, c@(CtxEsOut Hole var term)) = (ctx, c)
ctxHole (ctx, c@(CtxEsIn term var Hole)) = (ctx, c)
ctxHole (ctx1, (CtxAppL ctx2 term)) = ctxHole ((ctxSubCtx ctx1 (CtxAppL Hole term)), ctx2)
ctxHole (ctx1, (CtxAppR term ctx2)) = ctxHole ((ctxSubCtx ctx1 (CtxAppR term Hole)), ctx2)
ctxHole (ctx1, (CtxAbs var ctx2)) = ctxHole ((ctxSubCtx ctx1 (CtxAbs var Hole)), ctx2)
ctxHole (ctx1, (CtxEsOut ctx2 var term)) = ctxHole ((ctxSubCtx ctx1 (CtxEsOut Hole var term)), ctx2)
ctxHole (ctx1, (CtxEsIn term var ctx2)) = ctxHole ((ctxSubCtx ctx1 (CtxEsIn term var Hole)), ctx2)


trans :: State -> State
trans (State (App term1 term2) ctx iost sst Down) = State term1
    (ctxSubCtx ctx (CtxAppL Hole term2)) (Pi:iost) sst Down
trans (State (Abs var term) ctx iost@(x:xs) sst Down)
    |x == Pi = State term (ctxSubCtx ctx (CtxAbs var Hole)) xs sst Down
    |otherwise = let var2 = eSigName x
                     ctx2 = eSigCtx x
                     sig = eSigSig x
                 in (if (ctx == (sndCtx (bindCtx (Trip ctx2 Hole Down) var))) then (State (Var var2) ctx2 xs (sig ++ sst) Up) else final)
trans (State v@(Var var) ctx iost sst Down) = case inCtx of
                                                  (CtxAbs _ _) -> State (ctxSub inCtx v) ouCtx ((Sig sig):iost) remaining Up
                                                  (CtxEsOut ctx2 var term) -> State term (ctxSubCtx ouCtx (CtxEsIn (ctxSub ctx2 v) var Hole)) iost (sig:remaining) Down
    where
      trip = bindCtx (Trip ctx Hole Down) var
      ouCtx = sndCtx trip
      inCtx = fstCtx trip
      n = ctxLvl inCtx
      split = splitAt n sst
      init = fst split
      remaining = snd split
      sig = Signature var ctx init
trans (State (Es term var term2) ctx iost sst Down) = State term (ctxSubCtx ctx (CtxEsOut Hole var term2)) iost sst Down
trans (State term ctx iost sst Up) = case (ctxHole (Hole, ctx)) of (Hole, Hole) -> final
                                                                   (ctx2, (CtxAppL Hole term2)) -> (if (head iost == Pi)
                                                                                                        then (State (App term term2) ctx2 (tail iost) sst Up)
                                                                                                        else (State term2 (ctxSubCtx ctx2 (CtxAppR term Hole))
                                                                                                                (tail iost) ((signature (head iost)):sst) Down))
                                                                   (ctx2, (CtxAppR term2 Hole)) -> (State term2 (ctxSubCtx ctx2 (CtxAppL Hole term))
                                                                                                    ((Sig (head sst)):iost) (tail sst) Down)
                                                                   (ctx2, (CtxAbs var Hole)) -> (State (Abs var term) ctx2 (Pi:iost) sst Up)
                                                                   (ctx2, (CtxEsOut Hole var term2)) -> (State (Es term var term2) ctx2 iost sst Up)
                                                                   (ctx2, (CtxEsIn term2 var Hole)) -> (State (Var (sigName (head sst))) (sigCtx (head sst)) iost 
                                                                                                        ((sigSig (head sst)) ++ (tail sst)) Up) -- TODO: verify condition
trans _ = final




final :: State
final = State (Var "Final") Hole [] [] Up

run :: String -> Int -> IO ()
run s n = do
    writeFile "run.run" ""
    let e = parseExpr s
    case e of
        Left err -> putStr "err"
        Right ex -> appendFile "run.run" (showTable [ ColDesc center "Term"  left  (show . trm)
                     , ColDesc center "Context" left  (show . ctx)
                     , ColDesc center "IO Stack"  left (show . iost)
                     , ColDesc center "Sig Stack"  left (show . sst)
                     , ColDesc center "Polarity"  left (show . pol)
                     ] (running [(State ex Hole (pList n) [] Down)]))

runn :: String -> Int -> Int -> IO ()
runn s n m = do
    writeFile "run.run" ""
    let e = parseExpr s
    case e of
        Left err -> putStr "err"
        Right ex -> appendFile "run.run" (showTable [ ColDesc center "Term"  left  (show . trm)
                     , ColDesc center "Context" left  (show . ctx)
                     , ColDesc center "IO Stack"  left (show . iost)
                     , ColDesc center "Sig Stack"  left (show . sst)
                     , ColDesc center "Polarity"  left (show . pol)
                     ] (runningn m [(State ex Hole (pList n) [] Down)]))




pList :: Int -> [ExtSig]
pList 0 = []
pList n = Pi:(pList (n-1))

running :: [State] -> [State]
running xs
        | last xs == final = xs
        | otherwise = running (xs ++ [(trans (last xs))])


runningn :: Int -> [State] -> [State]
runningn 0 xs = xs
runningn n xs
        | last xs == final = xs
        | otherwise = runningn (n-1) (xs ++ [(trans (last xs))])

