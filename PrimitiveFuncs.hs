{-# LANGUAGE ExistentialQuantification #-}

module PrimitiveFuncs where

import Helper
import LanDef

import Data.Map hiding (map, null, take, drop)
import Control.Monad.Except
import Data.Char

primitiveFuncEnv :: Map String LanVal
primitiveFuncEnv = fromList [("get", Func "get" [("xs",TList), ("ys",TInt)] Any []),
                            ("head", Func "head" [("xs",TList)] Any []),
                            ("tail", Func "tail" [("xs",TList)] TList []),
                            ("concat", Func "concat" [("xs",Concat), ("ys",Concat)] TList []),
                            ("not", Func "not" [("x",TBool)] TBool []),
                            ("update", Func "update" [("xs",TList),("i",TInt),("x",Any)] TList []),
                            ("makeList", Func "makeList" [("x",Any),("i",TInt)] TList []),
                            ("length", Func "length" [("xs",Concat)] TInt []),
                            ("sqrt", Func "sqrt" [("x",Num)] TFloat []),
                            ("toInt", Func "toInt" [("x",TFloat)] TInt []),
                            ("toFloat", Func "toFloat" [("x",TInt)] TFloat []),
                            ("append", Func "append" [("xs",TList),("x",Any)] TList []),
                            ("remove", Func "remove" [("xs",TList),("i",TInt)] TList []),
                            ("charAt", Func "charAt" [("str",TString),("i",TInt)] TChar []),
                            ("ord", Func "ord" [("c",TChar)] TInt []),
                            ("chr", Func "chr" [("i",TInt)] TChar []),
                            ("makeString", Func "makeString" [("str",TString),("i",TInt)] TString []),
                            ("toString", Func "toString" [("x",Any)] TString []),
                            ("print", Func "print" [("str",Any)] TString []),
                            ("println", Func "println" [("str",Any)] TString [])]

primitiveFuncs :: Map String ([LanVal] -> IOThrowsError LanVal)
primitiveFuncs = fromList [("get", lget),
                            ("head", lhead),
                            ("tail", ltail),
                            ("concat", lconcat),
                            ("not", lnot),
                            ("update", lupdate),
                            ("makeList", lmakeList),
                            ("length", llength),
                            ("sqrt", lsqrt),
                            ("toInt", ltoInt),
                            ("toFloat", ltoFloat),
                            ("append", lappend),
                            ("remove", lremove),
                            ("charAt", lcharAt),
                            ("ord", lord),
                            ("chr", lchr),
                            ("makeString", lmakeString),
                            ("toString", ltoString),
                            ("print", lprint),
                            ("println", lprintln)]

lget :: [LanVal] -> IOThrowsError LanVal
lget [List ls, Int i] = if i>=length ls || i < 0
                        then return $ throwError $ IndOutRange (LitVal $ List ls) i
                        else  return $ return $ ls !! i
lget [List _, x] = return $ throwError $ TypeMismatch "Int" (LitVal x)
lget [x, _] = return $ throwError $ TypeMismatch "List" (LitVal x)
lget ls = return $ throwError $ NumArgs 2 (map LitVal ls)

lhead :: [LanVal] -> IOThrowsError LanVal
lhead [ls] = lget [ls, Int 0]
lhead ls = return $ throwError $ NumArgs 1 (map LitVal ls)

ltail :: [LanVal] -> IOThrowsError LanVal
ltail [List ls] = if null ls then return $ throwError $ Runtime "Cannot get tail of null list" else return $ return $ List (tail ls)
ltail [x] = return $ throwError $ TypeMismatch "List" (LitVal x)
ltail ls = return $ throwError $ NumArgs 2 (map LitVal ls)

lconcat :: [LanVal] -> IOThrowsError LanVal
lconcat [List xs, List ys] = return $ return $ List (xs++ys)
lconcat [String xs, String ys] = return $ return $ String (xs++ys)
lconcat [List xs, String ys] = return $ throwError $ TypeMismatch "List" (LitVal $ String ys)
lconcat [String xs, List ys] = return $ throwError $ TypeMismatch "String" (LitVal $ List ys)

lnot :: [LanVal] -> IOThrowsError LanVal
lnot [Bool b] = return $ return $ Bool (not b)
lnot [x] = return $ throwError $ TypeMismatch "Bool" (LitVal x)
lnot ls = return $ throwError $ NumArgs 1 (map LitVal ls)

lupdate :: [LanVal] -> IOThrowsError LanVal
lupdate [List ls, Int i, x] = if i<0 || i>=length ls then return $ throwError $ IndOutRange (LitVal $ List ls) i
                                else return $ return $ List (take i ls ++ [x] ++ drop (i+1) ls)

lappend :: [LanVal] -> IOThrowsError LanVal
lappend [List ls, x] = return $ return $ List (ls++[x])

lremove :: [LanVal] -> IOThrowsError LanVal
lremove [List ls, Int i] = if i<0 || i>=length ls then return $ throwError $ IndOutRange (LitVal $ List ls) i
                            else return $ return $ List (take i ls ++ drop (i+1) ls)

lmakeList :: [LanVal] -> IOThrowsError LanVal
lmakeList [x, Int i] = if i<0 then return $ throwError $ Runtime "Cannot make list with negative size"
                        else return $ return $ List (take i (repeat x))

llength :: [LanVal] -> IOThrowsError LanVal
llength [List ls] = return $ return $ Int (length ls)
llength [String ls] = return $ return $ Int (length ls)

lsqrt :: [LanVal] -> IOThrowsError LanVal
lsqrt [Int x] = if x<0 then return $ throwError $ MathError ("Cannot take square root of negative number: " ++ show x)
                    else return $ return $ Float (sqrt (fromIntegral x))
lsqrt [Float x] = if x<0 then return $ throwError $ MathError ("Cannot take square root of negative number: " ++ show x)
                    else return $ return $ Float (sqrt x)

ltoFloat :: [LanVal] -> IOThrowsError LanVal
ltoFloat [Int x] = return $ return $ Float (fromIntegral x)

ltoInt :: [LanVal] -> IOThrowsError LanVal
ltoInt [Float x] = return $ return $ Int (round x)

lcharAt :: [LanVal] -> IOThrowsError LanVal
lcharAt [String str, Int i] = if i<0 || i>=length str then return $ throwError $ IndOutRange (LitVal $ String str) i
                                else return $ return $ Char (str!!i)

lord :: [LanVal] -> IOThrowsError LanVal
lord [Char c] = return $ return $ Int (ord c)

lchr :: [LanVal] -> IOThrowsError LanVal
lchr [Int i] = return $ return $ Char (chr i)

lmakeString :: [LanVal] -> IOThrowsError LanVal
lmakeString [String str, Int i] = return $ return $ String (concat $ take i (repeat str))

ltoString :: [LanVal] -> IOThrowsError LanVal
ltoString [Int x] = return $ return $ String (show x)
ltoString [Float x] = return $ return $ String (show x)
ltoString [Char x] = return $ return $ String (show x)
ltoString [Bool x] = return $ return $ String (show x)
ltoString [String x] = return $ return $ String x
ltoString [List ls] = do
                        res <- mapM (ltoString . (:[])) ls
                        if any isLeft res then return $ head (dropWhile (not.isLeft) res)
                        else return $ return $ String $ joinBy ", " $ map ((\(String x)->x).extractVal) res

lprint' :: [LanVal] -> IOThrowsError LanVal
lprint' [String str] = do
                        putStr str
                        return $ return $ String str

lprintln' :: [LanVal] -> IOThrowsError LanVal
lprintln' [String str] = do
                            putStrLn str
                            return $ return $ String str

lprint :: [LanVal] -> IOThrowsError LanVal
lprint [x] = do
                    res <- ltoString [x]
                    case res of
                        Left err -> return $ throwError err
                        Right str -> lprint' [str]

lprintln :: [LanVal] -> IOThrowsError LanVal
lprintln [x] = do
                    res <- ltoString [x]
                    case res of
                        Left err -> return $ throwError err
                        Right str -> lprintln' [str]