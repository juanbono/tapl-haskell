{-|
Module      : Language.Untyped.Context
Description : Definition of the Context of evaluation and some useful functions.
Copyright   : (c) Juan Gabriel Bono, 2016
License     : BSD3
Maintainer  : juanbono94@gmail.com

-}
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
  , pickFreshName
  , termSubstTop
  ) where

import           Language.Untyped.Syntax

-- | Just an alias for 'String'
type Name = String

-- | It doesn't carry any useful information.
data Binding
  = NameBind
  deriving (Show)

-- | It's represented by a list of names and associated bindings.
type Context = [(Name, Binding)]

-- | An empty context.
emptyContext :: Context
emptyContext = []

-- | Appends the ('Name', 'Binding') tuple to the 'Context'.
addBinding :: Context -> Name -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

-- | Adds a 'Name' to a given 'Context'
addName :: Context -> Name -> Context
addName ctx name = addBinding ctx name NameBind

-- | Applies a function to the given context and returns another context.
modifyContext :: (Context -> Context) -> Context -> Context
modifyContext f ctx = f ctx

-- | Checks if a given 'Name' is bound within a 'Context'.
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

