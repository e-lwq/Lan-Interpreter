{-# LANGUAGE ExistentialQuantification #-}

module PrimitiveFuncs where

import Helper
import LanDef

import Data.Map hiding (map, null, take, drop)
import Control.Monad.Except

primitiveFuncEnv :: Map String LanVal
primitiveFuncEnv = fromList [("get", Func "get" [("xs",TList), ("ys",TInt)] Any []),
                            ("head", Func "head" [("xs",TList)] Any []),
                            ("tail", Func "tail" [("xs",TList)] TList []),
                            ("concat", Func "concat" [("xs",TList), ("ys",TList)] TList []),
                            ("not", Func "not" [("x",TBool)] TBool []),
                            ("update", Func "update" [("xs",TList),("i",TInt),("x",Any)] TList []),
                            ("makelist", Func "makelist" [("x",Any),("i",TInt)] TList []),
                            ("length", Func "length" [("xs",TList)] TInt [])]

primitiveFuncs :: Map String ([LanVal] -> ThrowsError LanVal)
primitiveFuncs = fromList [("get", lget),
                            ("head", lhead),
                            ("tail", ltail),
                            ("concat", lconcat),
                            ("not", lnot),
                            ("update", lupdate),
                            ("makelist", lmakelist),
                            ("length", llength)]

lget :: [LanVal] -> ThrowsError LanVal
lget [List ls, Int i] = if i>=length ls || i < 0
                        then throwError $ IndOutRange (LitVal $ List ls) i
                        else  return $ ls !! i
lget [List _, x] = throwError $ TypeMismatch "Int" (LitVal x)
lget [x, _] = throwError $ TypeMismatch "List" (LitVal x)
lget ls = throwError $ NumArgs 2 (map LitVal ls)

lhead :: [LanVal] -> ThrowsError LanVal
lhead [ls] = lget [ls, Int 0]
lhead ls = throwError $ NumArgs 1 (map LitVal ls)

ltail :: [LanVal] -> ThrowsError LanVal
ltail [List ls] = if null ls then throwError $ Runtime "Cannot get tail of null list" else return $ List (tail ls)
ltail [x] = throwError $ TypeMismatch "List" (LitVal x)
ltail ls = throwError $ NumArgs 2 (map LitVal ls)

lconcat :: [LanVal] -> ThrowsError LanVal
lconcat [List xs, List ys] = return $ List (xs++ys)
lconcat [List _, y] = throwError $ TypeMismatch "List" (LitVal y)
lconcat [x, _] = throwError $ TypeMismatch "List" (LitVal x)
lconcat ls = throwError $ NumArgs 2 (map LitVal ls)

lnot :: [LanVal] -> ThrowsError LanVal
lnot [Bool b] = return $ Bool (not b)
lnot [x] = throwError $ TypeMismatch "Bool" (LitVal x)
lnot ls = throwError $ NumArgs 1 (map LitVal ls)

lupdate :: [LanVal] -> ThrowsError LanVal
lupdate [List ls, Int i, x] = if i<0 || i>=length ls then throwError $ IndOutRange (LitVal $ List ls) i
                                else return $ List (take i ls ++ [x] ++ drop (i+1) ls)
-- supposedly, need to exhaustively consider other cases as well

lmakelist :: [LanVal] -> ThrowsError LanVal
lmakelist [x, Int i] = if i<0 then throwError $ Runtime "Cannot make list with negative size"
                        else return $ List (take i (repeat x))

llength :: [LanVal] -> ThrowsError LanVal
llength [List ls] = return $ Int (length ls)