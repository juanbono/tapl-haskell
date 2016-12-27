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
  , Info (..)
  ) where

-- | Data type for lambda terms
data Term
  -- | Contains the De Brujin index and the size of the current context.
  = TmVar Info Int Int
  -- | A String represents the bound variable within the term.
  | TmAbs Info String Term
  -- |  Reifies the application of one term to another.
  | TmApp Info Term Term
  deriving (Show)

-- | Data type that holds parsing information.
data Info
  = Info { row :: Int -- ^ row number
         , col :: Int -- ^ column number
         }
  deriving (Show)
