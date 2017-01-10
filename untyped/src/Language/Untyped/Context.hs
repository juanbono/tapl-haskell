{-|
Module      : Language.Untyped.Context
Description : Definition of the Context of evaluation and some useful functions.
Copyright   : (c) Juan Gabriel Bono, 2016
License     : BSD3
Maintainer  : juanbono94@gmail.com

-}
module Language.Untyped.Context
  ( Context
  , Binding (..)
  , CtxException (..)
  , Name
  , addName
  , toIndex
  , fromIndex
  , emptyContext
  , pickFreshName
  , termSubstTop
  ) where

import           Control.Exception
import           Data.Either
import           Data.Typeable
import           Language.Untyped.Syntax

-- == Definitions

-- | The variable's names are just an alias for 'String'
type Name = String

-- | It doesn't carry any useful information.
data Binding
  = NameBind
  deriving (Show, Eq)

data CtxException
  = NotFound
  | Unbound Name
  deriving (Typeable)

instance Show CtxException where
  show NotFound = "Variable lookup failure"
  show (Unbound name) = "UnboundException: Identifier " ++ name ++ " is unbound."

instance Exception CtxException

-- | It's represented by a list of names and associated bindings.
type Context = [(Name, Binding)]

-- == Context's Management

-- | An empty context.
emptyContext :: Context
emptyContext = []

-- | Appends the ('Name', 'Binding') tuple to the 'Context'.
addBinding :: Context -> Name -> Binding -> Context
addBinding ctx x bind = (x, bind) : ctx

-- | Adds a 'Name' to a given 'Context'
addName :: Context -> Name -> Context
addName ctx name = if isNameBound ctx name -- otra def addBinding ctx name NameBind
                   then ctx
                   else addBinding ctx name NameBind

-- | Checks if a given 'Name' is bound within a 'Context'.
isNameBound :: Context -> Name -> Bool
isNameBound ctx name = elem name . map fst $ ctx

-- | Checks if a given 'Context' contains a 'Name', in that case appends one or more "'".
-- In any other case returns a tuple containing the context with the new name and the name as 2nd component.
pickFreshName :: Context -> Name -> (Context, Name)
pickFreshName ctx x
  | isNameBound ctx x = pickFreshName ctx (x ++ "'")
  | otherwise = (addName ctx x, x)

-- |
fromIndex :: Context -> Int -> Either CtxException Name
fromIndex ctx index
  | length ctx < index = Left NotFound
  | otherwise = Right $ fst (ctx !! index)

-- |
toIndex :: Context -> Name -> Either CtxException Int
toIndex [] name = throw $ Unbound name
toIndex ((y, _):rest) name
  | name == y    = Right 0
  | otherwise = (+1) <$> toIndex rest name

-- * --

--
removeNames :: Context -> Term -> NamelessTerm
removeNames = undefined

restoreNames :: Context -> NamelessTerm -> Term
restoreNames = undefined
--
-- == Shifting

-- |
termMap :: Num t => (t -> Int -> NamelessTerm) -> t -> NamelessTerm -> NamelessTerm
termMap onvar c term
  = let walk c (NmVar index)     = onvar c index
        walk c (NmAbs name term) = NmAbs name (walk (c + 1) term)
        walk c (NmApp t1 t2)     = NmApp (walk c t1) (walk c t2)
    in walk c term

-- |
termShiftAbove :: Int -> Int -> NamelessTerm -> NamelessTerm
termShiftAbove d c term = termMap f c term
  where
    f c x
      | x >= c = NmVar (x + d)
      | otherwise = NmVar x

-- |
termShift :: Int -> NamelessTerm -> NamelessTerm
termShift d term = termShiftAbove d 0 term

-- * --

-- == Substitution

-- |
termSubst :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
termSubst j t1 t2 = termMap f 0 t2
  where
    f c x
      | x == j + c = termShift c t1
      | otherwise = NmVar x

-- |
termSubstTop :: NamelessTerm -> NamelessTerm -> NamelessTerm
termSubstTop t1 t2 = termShift (-1) $ termSubst 0 (termShift 1 t1) t2

-- * --
