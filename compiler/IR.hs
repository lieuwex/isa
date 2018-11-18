{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, LambdaCase #-}
module IR(buildIR) where

import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe

import AST


type Id = Int
type Imm = Int64

data IR = IR (Map.Map Id BB) (Map.Map Name Id)
  deriving (Show, Eq)

data BB = BB Id [IRIns] IRTerm
  deriving (Show, Eq)

data IRIns
    = IArith ArithOp Reg Reg Reg
    | IMov Reg Reg
    | ILi Reg Imm
    | ILoad Int Reg Reg
    | IStore Int Reg Reg
    | ICall Reg Id
  deriving (Show, Eq)

data IRTerm
    = TJmp Id
    | TBr Cond Reg JDest
    | TUnreachable
  deriving (Show, Eq)

data ArithOp = AAdd | ASub | AMul | ADiv
             | ANot | AAnd | AOr | AXor | ASll | ASlr | ASar
  deriving (Show, Eq)

data Cond = CZ | CNZ
  deriving (Show, Eq)

data JDest = JLabel Id | JReg Reg
  deriving (Show, Eq)

data Reg = RPC | RSP | RLink | RRet | Reg Int
  deriving (Show, Eq)


newtype BM a = BM { runBM :: StateT BuildState (Except String) a }
  deriving (Functor, Applicative, Monad, MonadState BuildState)

data BuildState =
    BuildState
        { bsBBMap :: Map.Map Id BB
        , bsFuncMap :: Map.Map Name Id
        , bsLocalVars :: [Map.Map Name Reg]
        -- , bsStackOffsets :: Int
        , bsCurrent :: Id
        , bsNext :: Id }

initBuildState :: BuildState
initBuildState =
    BuildState
        { bsBBMap = Map.empty
        , bsFuncMap = Map.empty
        , bsLocalVars = []
        -- , bsStackOffsets = [0]
        , bsCurrent = (-1)
        , bsNext = 0 }

execBM :: BM a -> BuildState -> Either String BuildState
execBM m st = runExcept $ execStateT (runBM m) st

genId :: BM Id
genId = state $ \s -> (bsNext s, s { bsNext = bsNext s + 1 })

genReg :: BM Reg
genReg = Reg <$> genId

registerFunc :: Name -> Id -> BM ()
registerFunc name i = modify $ \s -> s { bsFuncMap = Map.insert name i (bsFuncMap s) }

switchBB :: Id -> BM ()
switchBB i = modify $ \s -> s { bsCurrent = i }

newBBStay :: BM Id
newBBStay = do
    i <- genId
    modify $ \s -> s { bsBBMap = Map.insert i (BB i [] TUnreachable) (bsBBMap s) }
    return i

newBB :: BM Id
newBB = do
    i <- newBBStay
    switchBB i
    return i

modifyBB :: (BB -> BB) -> BM ()
modifyBB f = modify $ \s -> s { bsBBMap = Map.adjust f (bsCurrent s) (bsBBMap s) }

-- pushScope :: [(Name, Int)] -> BM ()
-- pushScope pairs = do
--     prevOff <- gets (head . bsStackOffsets)
--     let offsets = tail $ scanl (+) prevOff (map snd pairs)
--         newOff = last offsets
--         mp = Map.fromList (zip (map fst pairs) offsets)
--     modify $ \s -> s { bsStackOffsets = newOff : bsStackOffsets s
--                      , bsLocalVars = mp : bsLocalVars s }

-- popScope :: BM ()
-- popScope = modify $ \s -> s { bsLocalVars = tail $ bsLocalVars s
--                             , bsStackOffsets = tail $ bsStackOffsets s }

pushScope :: [(Name, Reg)] -> BM ()
pushScope pairs = modify $ \s -> s { bsLocalVars = Map.fromList pairs : bsLocalVars s }

popScope :: BM ()
popScope = modify $ \s -> s { bsLocalVars = tail $ bsLocalVars s }

localVar :: Name -> BM (Maybe Reg)
localVar name = do
    stk <- gets bsLocalVars
    case catMaybes $ map (Map.lookup name) stk of
        [] -> return Nothing
        (r:_) -> return (Just r)

addIns :: IRIns -> BM ()
addIns ins = modifyBB $ \(BB bid inss term) -> BB bid (inss ++ [ins]) term

setTerm :: IRTerm -> BM ()
setTerm term = modifyBB $ \(BB bid inss _) -> BB bid inss term

class Argument a where
    intoRegister :: a -> BM Reg

instance Argument Reg where
    intoRegister = return

instance Argument Imm where
    intoRegister imm = do
        r <- genReg
        addIns $ ILi r imm
        return r

iPush :: Argument a => Int -> a -> BM ()
iPush sz arg = do
    r <- intoRegister arg
    r8 <- genReg
    addIns $ ILi r8 8
    addIns $ IArith ASub RSP RSP r8
    addIns $ IStore sz RSP r

iPop :: Int -> Reg -> BM ()
iPop sz r = do
    addIns $ ILoad sz r RSP
    r8 <- genReg
    addIns $ ILi r8 8
    addIns $ IArith AAdd RSP RSP r8

iRet :: BM ()
iRet = addIns $ IMov RPC RLink


buildIR :: Program -> Either String IR
buildIR prog = do
    st <- execBM (compileProgram prog) initBuildState
    return $ IR (bsBBMap st) (bsFuncMap st)

compileProgram :: Program -> BM ()
compileProgram (Program decls) = do
    rootBBs <- sequence (replicate (length decls) newBBStay)
    forM_ (zip rootBBs decls) $ \(bid, Decl name _) -> registerFunc name bid

    forM_ (zip rootBBs decls) $ \(bid, Decl _ expr) -> do
        endbb <- newBBStay

        switchBB bid
        iPush 64 RLink
        retreg <- compileExpr expr endbb

        switchBB endbb
        iPop 64 RLink
        addIns $ IMov RRet retreg
        iRet

compileExpr :: Expr -> Id -> BM Reg
compileExpr (ERef name) endbb = do
    localVar name >>= \case
        Just r -> do
            setTerm $ TJmp endbb
            return r
        Nothing -> do
            fail $ "Use of undeclared variable '" ++ name ++ "'"
            -- mp <- gets bsFuncMap
            -- case Map.lookup name mp of
            --     Just i -> do
            --         r <- genReg
            --         addIns $ ICall r i
            --         return r
            --     Nothing ->
            --         fail $ "Use of undeclared variable '" ++ name ++ "'"
compileExpr (EInt num) endbb = do
    r <- genReg
    addIns $ ILi r (fromIntegral num)
    setTerm $ TJmp endbb
    return r
compileExpr (ELet pairs body) endbb = do
    rs <- forM pairs $ \(_, expr) -> do
            bb <- newBBStay
            r <- compileExpr expr bb
            switchBB bb
            return r

    pushScope (zip (map fst pairs) rs)
    res <- compileExpr body endbb
    popScope
    return res
compileExpr (ECall (ECall (ERef name) arg1) arg2) endbb
    | Just aop <- builtinArith name = do
        bb1 <- newBBStay
        r1 <- compileExpr arg1 bb1
        switchBB bb1

        bb2 <- newBBStay
        r2 <- compileExpr arg2 bb2
        switchBB bb2

        r <- genReg
        addIns $ IArith aop r r1 r2
        setTerm $ TJmp endbb
        return r
    | otherwise =
        gets bsFuncMap >>= \mp -> case Map.lookup name mp of
            Just _ -> error "UNIMPLEMENTED"
            Nothing -> fail $ "Use of undeclared function '" ++ name ++ "'"
-- compileExpr (ECall func arg) endbb = do
--     bb1 <- newBBStay
--     rfunc <- compileExpr func endbb
--     switchBB bb1

--     bb2 <- newBBStay
--     rarg <- compileExpr arg bb2
--     switchBB bb2

--     ;

builtinArith :: String -> Maybe ArithOp
builtinArith "+" = Just AAdd
builtinArith "-" = Just ASub
builtinArith "*" = Just AMul
builtinArith "/" = Just ADiv
builtinArith "&" = Just AAnd
builtinArith "|" = Just AOr
builtinArith "^" = Just AXor
builtinArith ".<<" = Just ASll
builtinArith ".>>" = Just ASlr
builtinArith ".>>>" = Just ASar
builtinArith _ = Nothing
