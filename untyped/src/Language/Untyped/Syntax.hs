{-|
Module      : Language.Untyped.Syntax
Description : Definitions of terms for the untyped lambda calculus.
Copyright   : (c) Juan Gabriel Bono, 2016
License     : BSD3
Maintainer  : juanbono94@gmail.com

-}
module Language.Untyped.Syntax
  (
    Term (..)
  ) where

-- | Data type for lambda terms
data Term
  -- | Contains the De Brujin index of the variable
  = TmVar Int
  -- | A String represents the bound variable within the term.
  | TmAbs String Term
  -- |  Reifies the application of one term to another.
  | TmApp Term Term
  deriving (Show, Eq)
