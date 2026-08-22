module Evaluator where

import Helper
import LanDef
import PrimitiveOps
import PrimitiveFuncs

import Data.Map (Map, lookup, fromList, insert)
import Prelude hiding (lookup)
import Control.Monad.Except

-- Evaluate exprs, SHOULD NOT ALTER ENVIRONMENT except function applications
evalExpr :: LanExpr -> Exe LanVal
evalExpr (LitVal val) = return val
evalExpr (VarRef varname) = lookupVarExe varname
evalExpr (FuncApp funcname args) = do
                                    func <- lookupVarExe funcname
                                    apply func args
evalExpr (Op op e1 e2) = do
                            v1 <- evalExpr e1
                            v2 <- evalExpr e2
                            case lookup op primitiveOps of
                                Nothing -> throwExeError $ NotOp op
                                Just f -> case f v1 v2 of
                                            Left err -> throwExeError err
                                            Right res -> return res

lookupVar :: Env -> String -> Maybe LanVal
lookupVar [] varname = Nothing
lookupVar (env:es) varname = case lookup varname env of
                                Nothing -> lookupVar es varname
                                Just val -> Just val

lookupVarExe :: String -> Exe LanVal
lookupVarExe varname = Exe (\env -> case lookupVar env varname of
                                    Nothing -> return $ throwError $ VarNotFound varname
                                    Just val -> return $ return (val,env))

apply :: LanVal -> [LanExpr] -> Exe LanVal
apply (func@(Func name params retType body)) args = case lookup name primitiveFuncs of
                                                        Nothing -> do
                                                                    eval_args <- checkFuncApp func args
                                                                    enterBlock
                                                                    bindVarsExe (zip (map fst params) eval_args)
                                                                    res <- evalProg body
                                                                    exitBlock
                                                                    if checkType retType (getType res)
                                                                    then return res
                                                                    else throwExeError $ TypeMismatch ("Return type " ++ show retType) (LitVal res)
                                                        Just f -> do
                                                                    eval_args <- checkFuncApp func args
                                                                    r <- liftIO2Exe $ f eval_args
                                                                    return r

checkType :: LanType -> LanType -> Bool
checkType Any _ = True
checkType Num tp = tp==TInt || tp==TFloat
checkType Concat tp = tp==TList || tp==TString
checkType Ord tp = tp `elem` [TChar,TString,TInt,TFloat]
checkType a b = a==b

checkFuncApp :: LanVal -> [LanExpr] -> Exe [LanVal]
checkFuncApp (Func name params retType _) args | lp /= la = throwExeError $ NumArgs lp args
                                                    | otherwise = do
                                                                    eval_args <- mapM evalExpr args
                                                                    let {
                                                                        vs = zip args eval_args;
                                                                        ls = zip params vs;
                                                                        ls' = dropWhile (\((pn,pt),(ae,av))-> checkType pt (getType av)) ls;
                                                                    }
                                                                    if null ls' then return eval_args
                                                                    else throwExeError $ TypeMismatch (show $ snd $ fst $ head ls) (fst $ snd $ head ls)
    where
        lp = fromIntegral $ length params
        la = fromIntegral $ length args

--bindVars :: [(String, LanVal)] -> Env -> Env
--bindVars vars env = fromList vars : env

bindVarExe :: String -> LanVal -> Exe LanVal
bindVarExe varname val = Exe (\env -> if null env then return $ throwError EnvError
                                else case lookup varname (head env) of
                                        Just _ -> return $ throwError $ VarDefTwice varname
                                        Nothing -> return $ return (val, insert varname val (head env) : tail env))

bindVarsExe :: [(String, LanVal)] -> Exe [LanVal]
bindVarsExe vars = mapM (uncurry bindVarExe) vars

-- Evaluate Block
enterBlock :: Exe ()
enterBlock = Exe (\env -> return $ return ((),(fromList []):env))

exitBlock :: Exe ()
exitBlock = Exe (\env -> if null env then (return $ throwError EnvError) else return $ return ((), tail env))

setVarExe :: String -> LanVal -> Exe LanVal
setVarExe varname val = Exe (\env -> case setVar env varname val of
                                        Left err -> return $ throwError err
                                        Right res -> return $ return (val,res))

setVar :: Env -> String -> LanVal -> ThrowsError Env
setVar [] varname _ = throwError $ VarNotFound varname
setVar (env:es) varname val = case lookup varname env of
                                Nothing -> (env:) <$> setVar es varname val
                                Just _ -> return ((insert varname val env):es)

defFunc :: String -> [(String, LanType)] -> LanType -> [Block] -> Exe LanVal
defFunc funcname params retType body = Exe (\env -> case lookupVar env funcname of
                                                    Just _ -> return $ throwError $ VarDefTwice funcname
                                                    Nothing -> if null env then return $ throwError EnvError
                                                                else return $ return (f,insert funcname f (head env) : tail env))
                                                    where f = Func funcname params retType body

runForLoop1 :: LanVal -> String -> Int -> Int -> Program -> Exe LanVal
runForLoop1 lastval var s e prog = if s>=e then return lastval
                                    else do
                                            enterBlock
                                            bindVarExe var (Int s)
                                            newval <- evalProg prog
                                            exitBlock
                                            runForLoop1 newval var (s+1) e prog

runForLoop2 :: LanVal -> String -> [LanVal] -> Program -> Exe LanVal
runForLoop2 lastval _ [] _ = return lastval
runForLoop2 _ var (l:ls) prog = do
                                    enterBlock
                                    bindVarExe var l
                                    newval <- evalProg prog
                                    exitBlock
                                    runForLoop2 newval var ls prog

runWhileLoop :: LanVal -> LanExpr -> Program -> Exe LanVal
runWhileLoop lastval cond body = do
                            res <- evalExpr cond
                            case res of
                                Bool b -> if not b
                                        then return lastval
                                        else do
                                                enterBlock
                                                newval <- evalProg body
                                                exitBlock
                                                runWhileLoop newval cond body
                                _ -> throwExeError $ TypeMismatch "Bool" cond

evalBlock :: Block -> Exe LanVal
evalBlock (DecVar varname vartype expr) = do
                                            val <- evalExpr expr
                                            if getType val /= vartype
                                            then throwExeError $ TypeMismatch (show vartype) expr
                                            else bindVarExe varname val
evalBlock (DecVar2 varname expr) = do
                                    val <- evalExpr expr
                                    bindVarExe varname val
evalBlock (SetVar varname expr) = do
                                    newval <- evalExpr expr
                                    setVarExe varname newval
evalBlock (DefFunc funcname params retType body) = do
                                                    val <- defFunc funcname params retType body
                                                    return val
evalBlock (Cond cond then_body else_body) = do
                                                res <- evalExpr cond
                                                case res of
                                                    Bool b -> do
                                                                enterBlock
                                                                val <- if b then evalProg then_body else evalProg else_body
                                                                exitBlock
                                                                return val
                                                    _ -> throwExeError $ TypeMismatch "Bool" cond
evalBlock (ForLoop1 var start end body) = do
                                            s' <- evalExpr start
                                            e' <- evalExpr end
                                            case s' of
                                                Int s -> case e' of
                                                            Int e -> do
                                                                        res <- runForLoop1 (Int 0) var s e body
                                                                        return res
                                                            _ -> throwExeError $ TypeMismatch "Int" (LitVal e')
                                                _ -> throwExeError $ TypeMismatch "Int" (LitVal s')
evalBlock (ForLoop2 var lsexpr body) = do
                                        ls <- evalExpr lsexpr
                                        case ls of
                                            List xs -> runForLoop2 (Int 0) var xs body
                                            _ -> throwExeError $ TypeMismatch "List" lsexpr
evalBlock (WhileLoop cond body) = runWhileLoop (Int 0) cond body
evalBlock (Expr expr) = evalExpr expr

-- Evaluate Program
evalProg :: Program -> Exe LanVal
evalProg [] = return $ Int 0
evalProg [block] = evalBlock block
evalProg (b:bs) = evalBlock b >> evalProg bs