{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Preprocess(preprocess) where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.List

import Debug.Trace

import AST
import ASTUtil


newtype PM a = PM (WriterT [Decl] (State PMState) a)
  deriving (Functor, Applicative, Monad, MonadWriter [Decl], MonadState PMState)

newtype PMState = PMState { pmNext :: Int }

initState :: PMState
initState = PMState 0

evalPM :: PM a -> (a, [Decl])
evalPM (PM wr) = evalState (runWriterT wr) initState

genId :: PM Int
genId = state $ \s -> (pmNext s, s { pmNext = pmNext s + 1 })


preprocess :: Program -> Program
preprocess prog =
    let (Program decls, newdecls) = evalPM (preProgram prog)
    in Program (decls ++ newdecls)

preProgram :: Program -> PM Program
preProgram (Program decls) = Program <$> mapM preDecl decls

preDecl :: Decl -> PM Decl
preDecl (Decl name expr) = Decl name <$> preExpr expr

preExpr :: Expr -> PM Expr
preExpr (ECall e1 e2) = ECall <$> preExpr e1 <*> preExpr e2
preExpr e@(ERef _) = return e
preExpr e@(EInt _) = return e
preExpr orig@(ELambda name body) =
    let frees = traceShowId $ freeVars body \\ [name]
    in case frees of
        [] -> do
            num <- genId
            let bodyname = "$lam" ++ show num
            body' <- preExpr body
            tell [Decl bodyname body']
            return (ERef bodyname)
        _ -> return orig
preExpr (ELet pairs expr) =
    ELet <$> mapM (\(name, e) -> (name,) <$> preExpr e) pairs <*> preExpr expr
