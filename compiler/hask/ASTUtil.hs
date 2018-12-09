module ASTUtil where

import Data.List
import qualified Data.Set as Set

import AST


freeVars :: Expr -> [Name]
freeVars expr = nub $ go expr Set.empty
  where
    go :: Expr -> Set.Set Name -> [Name]
    go (ECall e1 e2) bound = go e1 bound ++ go e2 bound
    go (ERef name) bound = if Set.member name bound then [] else [name]
    go (EInt _) _ = []
    go (ELambda name e) bound = go e (Set.insert name bound)
    go (ELet pairs e) bound = go e (foldl (\s (name, _) -> Set.insert name s) bound pairs)
