module Language.Untyped.Context
  (
    Context
  , Binding
  , Name
  , addName
  , addBinding
  , modifyContext
  , toIndex
  , fromIndex
  , emptyContext
  , showTerm
  ) where

import           Language.Untyped.Syntax
import           Printcess.PrettyPrinting

type Name = String
data Binding
  = NameBind deriving (Show)
type Context = [(Name, Binding)]

emptyContext :: Context
emptyContext = []

addBinding :: Context -> Name -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

addName :: Context -> Name -> Context
addName ctx name = addBinding ctx name NameBind

modifyContext f ctx = f ctx

isNameBound :: Context -> Name -> Bool
isNameBound ctx name = elem name . map fst $ ctx

pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx x
  | isNameBound ctx x = pickFreshName ctx (x ++ "'")
  | otherwise = (addName ctx x, x)

fromIndex :: Context -> Int -> Name
fromIndex ctx i = fst $ ctx !! i

toIndex :: Context -> Name -> Int
toIndex [] x = error ("Identifier " ++ x ++ " is unbound.")
toIndex ((y, _):rest) x
  | x == y    = 0
  | otherwise = 1 + (toIndex rest x)

showTerm :: Context -> Term -> String
showTerm ctx (TmVar _ index n)
  | length ctx == n = fromIndex ctx index
  | otherwise = "Error: bad index"
showTerm ctx (TmAbs _ name term)
  = let (ctx', name') = pickFreshName ctx name
    in "(λ" ++ name' ++ "." ++ showTerm ctx' term ++ ")"
showTerm ctx (TmApp _ t1 t2)
  = "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"

termMap :: Num t => (Info -> t -> Int -> Int -> Term) -> t -> Term -> Term
termMap onvar c t
  = let walk c (TmVar info index n)   = onvar info c index n
        walk c (TmAbs info name term) = TmAbs info name (walk (c + 1) term)
        walk c (TmApp info t1 t2)     = TmApp info (walk c t1) (walk c t2)
    in walk c t

termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t = termMap f c t
  where
    f info c x n
      | x >= c = TmVar info (x + d) (n + d)
      | otherwise = TmVar info x (n + d)

termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

termSubst :: Int -> Term -> Term -> Term
termSubst j s t = termMap f 0 t
  where
    f info c x n
      | x == j + c = termShift c s
      | otherwise = TmVar info x n

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) $ termSubst 0 (termShift 1 s) t

