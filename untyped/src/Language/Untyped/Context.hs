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
  deriving (Show)

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
modifyContext f ctx = f ctx

-- | Checks if a given 'Name' is bound within a 'Context'.
isNameBound :: Context -> Name -> Bool -- testeado de forma villera
isNameBound ctx name = elem name . map fst $ ctx

-- | Checks if a given 'Context' contains a 'Name', in that case appends one or more "'".
-- In any other case returns a tuple containing the context with the new name and the name as 2nd component.
pickFreshName :: Context -> Name -> (Context, Name) --testeado villero
pickFreshName ctx x
  | isNameBound ctx x = pickFreshName ctx (x ++ "'")
  | otherwise = (addName ctx x, x)

-- |
fromIndex :: Context -> Int -> Either CtxException Name
fromIndex ctx index -- testeado villero
  | length ctx < index = Left $ NotFound --error "Variable lookup failure: offset: " ++ show info
  | otherwise = Right $ fst (ctx !! index)

-- | testeado villero
toIndex :: Context -> Name -> Either CtxException Int
toIndex [] name = throw $ Unbound name--error $ "Identifier " ++ name ++ " is unbound."
toIndex ((y, _):rest) name
  | name == y    = Right 0
  | otherwise = (+1) <$> (toIndex rest name)

-- * --

-- == Shifting

-- |
termMap :: Num t => (t -> Int -> Int -> Term) -> t -> Term -> Term
termMap onvar c t
  = let walk c (TmVar index n)   = onvar c index n
        walk c (TmAbs name term) = TmAbs name (walk (c + 1) term)
        walk c (TmApp t1 t2)     = TmApp (walk c t1) (walk c t2)
    in walk c t

-- |
termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d c t = termMap f c t
  where
    f c x n
      | x >= c = TmVar (x + d) (n + d)
      | otherwise = TmVar x (n + d)

-- |
termShift :: Int -> Term -> Term
termShift d t = termShiftAbove d 0 t

-- * --

-- == Substitution

-- |
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = termMap f 0 t
  where
    f c x n
      | x == j + c = termShift c s
      | otherwise = TmVar x n

-- |
termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) $ termSubst 0 (termShift 1 s) t

-- * --

-- tests
ctx1, ctx2, ctx3 :: Context
ctx1 = []

ctx2 = [("x", NameBind)]

ctx3 = [("x", NameBind), ("y", NameBind)]

ctxs = [ctx1, ctx2, ctx3]
