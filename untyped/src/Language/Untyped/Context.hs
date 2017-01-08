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
  , modifyContext
  , toIndex
  , fromIndex
  , emptyContext
  , pickFreshName
  , termSubstTop
  ) where

import           Language.Untyped.Syntax
import           Data.Either
import           Data.Typeable
import           Control.Exception

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
addName ctx name = addBinding ctx name NameBind

-- | Applies a function to the given context and returns another context.
modifyContext :: (Context -> Context) -> Context -> Context
modifyContext f = f

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

-- | testeado villero
toIndex :: Context -> Name -> Either CtxException Int
toIndex [] name = throw $ Unbound name
toIndex ((y, _):rest) name
  | name == y    = Right 0
  | otherwise = (+1) <$> toIndex rest name

-- * --

-- == Shifting

-- |
termMap :: Num t => (t -> Int -> Term) -> t -> Term -> Term
termMap onvar c term
  = let walk c (TmVar index)     = onvar c index
        walk c (TmAbs name term) = TmAbs name (walk (c + 1) term)
        walk c (TmApp t1 t2)     = TmApp (walk c t1) (walk c t2)
    in walk c term

-- |
termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c term = termMap f c term
  where
    f c x
      | x >= c = TmVar (x + d)
      | otherwise = TmVar x
-- |
termShift :: Int -> Term -> Term
termShift d term = termShiftAbove d 0 term

-- * --

-- == Substitution

-- |
termSubst :: Int -> Term -> Term -> Term
termSubst j t1 t2 = termMap f 0 t2
  where
    f c x
      | x == j + c = termShift c t1
      | otherwise = TmVar x

-- |
termSubstTop :: Term -> Term -> Term
termSubstTop t1 t2 = termShift (-1) $ termSubst 0 (termShift 1 t1) t2

-- * --
