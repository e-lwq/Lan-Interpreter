module Repl where

import Helper
import Parser
import LanDef

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (token, space, spaces, choice)

-- parse repl
data Repl = EmptyComm | Quit | Prev | LanExpr LanExpr | Run String | Format String | Cd String

fileChar :: [Char]
fileChar = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-./"

parseFilename :: Parser String
parseFilename = do
                    token1 $ char '"'
                    filename <- many (oneOf fileChar)
                    token1 $ char '"'
                    return filename

parseRun :: Parser Repl
parseRun = do
                token $ string "#run"
                filename <- parseFilename
                return $ Run filename

parseFormat :: Parser Repl
parseFormat = do
                token $ string "#format"
                filename <- parseFilename
                return $ Format filename

parseCd :: Parser Repl
parseCd = do
            token $ string "#cd"
            filepath <- parseFilename
            return $ Cd filepath

parsePrev :: Parser Repl
parsePrev = do
                (token $ string "#prev") </> (token $ string "#p")
                return Prev

parseQuit :: Parser Repl
parseQuit = ((token $ string "#quit") </> (token $ string "#q")) >> return Quit

parseREPL :: Parser Repl
parseREPL = parseRun </> parseFormat </> parseCd </> parsePrev </> parseQuit </> (parseExpr >>= return . LanExpr)