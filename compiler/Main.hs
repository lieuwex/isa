{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.Exit

import IR
import Parser
-- import Preprocess


handleError :: Either String a -> IO a
handleError (Left err) = die err
handleError (Right x) = return x

handleErrorShow :: Show e => Either e a -> IO a
handleErrorShow = handleError . either (Left . show) Right

entryFile :: String -> IO ()
entryFile fname = do
    src <- readFile fname
    prog <- handleErrorShow $ parseProgram fname src
    print prog
    -- let prog' = preprocess prog
    -- print prog'
    ir <- handleError $ buildIR prog
    print ir
    -- opt <- handleError $ optimise ir
    -- print opt
    -- asm <- handleError $ assemble opt
    -- print asm

main :: IO ()
main = do
    getArgs >>= \case
        [fname] -> entryFile fname
        _ -> do
            putStrLn "Usage: ./compiler <source.lam>"
            exitFailure
