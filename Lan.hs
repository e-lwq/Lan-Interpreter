{-# OPTIONS_GHC -Wno-tabs #-}

module Lan where

-- Driver File (imports from everything else)

import Helper
import LanDef
import Parser
import Evaluator

import Data.Map
import System.IO
import System.Environment
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (choice)
import GHC.IO.Handle

lan :: IO ()
lan = do
        inp <- readFile "input/input.txt"
        case readEvalProg inp of
                Left err -> putStrLn (show err)
                Right val -> putStrLn (show val)

r = readFile "input/input.txt" >>= putStrLn . show . readEvalExpr

nullEnv :: Env
nullEnv = []

startEnv :: Env
startEnv = [Data.Map.empty]

readLan :: Show a => Parser a -> String -> ThrowsError a
readLan p inp = case parse p "LAN" inp of
					Left err -> throwError $ Parser err
					Right res -> return res

readVal :: String -> ThrowsError LanVal
readVal = readLan parseVal

readExpr :: String -> ThrowsError LanExpr
readExpr = readLan parseExpr

readBlock :: String -> ThrowsError Block
readBlock = readLan parseBlock

readProg :: String -> ThrowsError Program
readProg = readLan parseProg

readEvalExpr :: String -> ThrowsError LanVal
readEvalExpr inp = readExpr inp >>= evalExpr nullEnv 

readEvalProg :: String -> Exe LanVal
readEvalProg prog = case readProg prog of
                        Left err -> throwExeError err
                        Right res -> evalProg res