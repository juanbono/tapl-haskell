{-|
Module      : Language.Untyped.Syntax
Description : Definitions of terms for the untyped lambda calculus.
Copyright   : (c) Juan Gabriel Bono, 2016
License     : BSD3
Maintainer  : juanbono94@gmail.com

-}
module Language.Untyped.Syntax
  ( NamelessTerm (..)
  , Term (..)
  ) where

import qualified Data.Set as Set

-- | Data type for nameless representations of lambda terms
data NamelessTerm
  -- | Contains the De Brujin index of the variable
  = NmVar Int
  -- | A String represents the bound variable within the term.
  | NmAbs String NamelessTerm
  -- |  Reifies the application of one term to another.
  | NmApp NamelessTerm NamelessTerm
  deriving (Show, Eq)

data Term
  = Var String
  | Abs String Term
  | App Term Term

-- todo:
-- substitution function for both representations

size :: Term -> Int
size (Var _)     = 1
size (Abs _ t)   = 1 + size t
size (App t1 t2) = size t1 + size t2

freeVars :: Term -> Set.Set String
freeVars (Var name)      = Set.singleton name
freeVars (Abs name term) = freeVars term `Set.difference` Set.singleton name
freeVars (App t1 t2)     = freeVars t1 `Set.union` freeVars t2
-- to test: |FV(t)| <= size(t)
