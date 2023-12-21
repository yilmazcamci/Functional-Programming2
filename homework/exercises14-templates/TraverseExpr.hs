{-# LANGUAGE InstanceSigs #-}
module TraverseExpr where

import Control.Monad.State -- or import State
import Data.Foldable
import Data.List 
import Data.Maybe
import qualified Data.Map as M

data Expr var = Var var | Lit Integer | Op BinOp (Expr var) (Expr var)
  deriving (Show,Eq)
data BinOp    = Add | Sub | Mul | Div
  deriving (Show,Eq)

instance Functor Expr where
  --fmap :: (a -> b) -> (Expr a -> Expr b)
  fmap f (Var x) = Var $ f x
  fmap f (Lit i) = Lit i
  fmap f (Op b e1 e2) = Op b (fmap f e1) (fmap f e2)

instance Foldable Expr where
  --foldMap :: (Monoid m) => (a -> m) -> Expr a -> m
  foldMap f (Var x) = f x
  foldMap f (Lit _) = mempty
  foldMap f (Op _ e1 e2) = foldMap f e1 <> foldMap f e2
  
  -- In plain English: folding an expression means folding 
  -- subexpressions if there are any (as is the case only
  -- for the Op constructor), then using <> from the Monoid m
  -- to combine these two fold results into one. There is 
  -- information ignored: in particular, since f cannot
  -- do anything with integers, we have to ignore the Int
  -- argument of the Lit constructor when folding it, and there
  -- is nothing sensible we can do with it.
  -- The same holds for Op: the BinOp argument cannot be used
  -- meaningfully since f does not take an argument related
  -- to BinOp.
  
  -- This also means that we cannot make Expr into a foldable
  -- such that we can construct an f such that foldMap f 
  -- evaluates the expression in the "natural way", that is,
  -- using information of the operator.

instance Traversable Expr where
  --traverse :: (Applicative f) => (a -> f b) -> Expr a -> f (Expr b)
  traverse f (Var x) = pure Var <*> f x 
  traverse f (Lit i) = pure Lit <*> pure i
  traverse f (Op b e1 e2) = pure (Op b) <*> traverse f e1 <*> traverse f e2

allVars :: (Ord a) => Expr a -> [a]
allVars = map head . group . sort . foldMap (:[])


renameVar :: String -> State (M.Map String Int) Int
renameVar name = do 
    vars <- get
    case M.lookup name vars of
        Just v -> return v
        Nothing-> do 
            let s = M.size vars in do
                put (M.insert name s vars)
                return s

renameAllVars :: Expr String -> State (M.Map String Int) (Expr Int)
renameAllVars = traverse renameVar

indexVars :: Expr String -> Expr Int
indexVars e = evalState (renameAllVars e) M.empty
