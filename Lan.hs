{-# OPTIONS_GHC -Wno-tabs #-}

module Lan where

import LanDef
import Parser

import System.IO
import System.Environment
import Text.ParserCombinators.Parsec hiding (choice)
import GHC.IO.Handle

lan :: IO ()
lan = do
        inp <- getLine
        putStrLn (readExpr inp)
        --main

r = readFile "input.txt" >>= putStrLn . readBlock

readLan :: Show a => Parser a -> String -> String
readLan p inp = case parse p "LAN" inp of
					Left err -> "Error: \n" ++ show err 
					Right res -> "Parsed: \n" ++ show res

readVal :: String -> String
readVal = readLan parseVal

readExpr :: String -> String
readExpr = readLan parseExpr

readBlock :: String -> String
readBlock = readLan parseBlock