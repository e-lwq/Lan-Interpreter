{-# OPTIONS_GHC -Wno-tabs #-}

module Lan where

-- Driver File (imports from everything else)

import Helper
import LanDef
import Parser
import Evaluator
import PrimitiveFuncs

import Data.Map
import System.IO
import System.Environment
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (choice)
import GHC.IO.Handle

lan :: IO ()
lan = do
        putStr "| Lan > "
        command <- getLine
        case parse parseREPL "LAN" command of
                Left err -> putStrLn "Invalid command" >> lan
                Right x -> case x of
                                Run filename -> runProg filename >> lan
                                LanExpr expr -> runExpr expr >> lan
                                Format filename -> printProg filename >> lan
                                Quit -> return ()

-- command line functions
runProg :: String -> IO ()
runProg filename = do
                        inp <- readFile filename
                        do
                                r <- runExe (readEvalProg (inp++"\n")) startEnv
                                case r of
                                        Left err -> putStrLn (show err)
                                        Right (val,_) -> return () --putStrLn (show val)

runExpr :: LanExpr -> IO ()
runExpr expr = do
                r <- runExe (evalExpr expr) startEnv
                case r of
                        Left err -> putStrLn (show err)
                        Right (res,_) -> putStrLn (show r)

printProg :: String -> IO ()
printProg filename = do
                        inp <- readFile filename
                        case readProg inp of
                                Left err -> putStrLn (show err)
                                Right prog -> putStrLn (showProg prog)

-- helper functions
nullEnv :: Env
nullEnv = []

startEnv :: Env
startEnv = [Data.Map.empty, primitiveFuncEnv]


readLan :: Show a => Parser a -> String -> ThrowsError a
readLan p inp = case parse p "LAN" inp of
					Left err -> throwError $ Parser err
					Right res -> return res

readProg :: String -> ThrowsError Program
readProg = readLan parseProg

readEvalProg :: String -> Exe LanVal
readEvalProg prog = case readProg prog of
                        Left err -> throwExeError err
                        Right res -> evalProg res