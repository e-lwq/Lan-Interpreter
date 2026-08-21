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
                                    Nothing -> throwError $ VarNotFound varname
                                    Just val -> return (val,env))

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
                                                                    case f eval_args of
                                                                        Left err -> throwExeError err
                                                                        Right res -> return res

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
bindVarExe varname val = Exe (\env -> if null env then throwError EnvError
                                else case lookup varname (head env) of
                                        Just _ -> throwError $ VarDefTwice varname
                                        Nothing -> return (val, insert varname val (head env) : tail env))

bindVarsExe :: [(String, LanVal)] -> Exe [LanVal]
bindVarsExe vars = mapM (uncurry bindVarExe) vars

-- Evaluate Block
exitBlock :: Exe ()
exitBlock = Exe (\env -> if null env then throwError EnvError else return ((), tail env))

enterBlock :: Exe ()
enterBlock = Exe (\env -> return ((),(fromList []):env))

--evalExpr2Exe :: LanExpr -> Exe LanVal
--evalExpr2Exe expr = Exe (\env -> case evalExpr env expr of
--                                    Left err -> throwError err
--                                    Right val -> return (val,env))

setVarExe :: String -> LanVal -> Exe LanVal
setVarExe varname val = Exe (\env -> case setVar env varname val of
                                        Left err -> throwError err
                                        Right res -> return (val,res))

setVar :: Env -> String -> LanVal -> ThrowsError Env
setVar [] varname _ = throwError $ VarNotFound varname
setVar (env:es) varname val = case lookup varname env of
                                Nothing -> (env:) <$> setVar es varname val
                                Just _ -> return ((insert varname val env):es)

defFunc :: String -> [(String, LanType)] -> LanType -> [Block] -> Exe LanVal
defFunc funcname params retType body = Exe (\env -> case lookupVar env funcname of
                                                    Just _ -> throwError $ VarDefTwice funcname
                                                    Nothing -> if null env then throwError EnvError
                                                                else return (f,insert funcname f (head env) : tail env))
                                                    where f = Func funcname params retType body

runForLoop1 :: LanVal -> String -> Int -> Program -> Exe LanVal
runForLoop1 lastval var e prog = do
                                    val <- lookupVarExe var
                                    case val of
                                        Int v -> if v==e then return lastval
                                                else evalProg prog >>= \newval -> setVarExe var (Int (v+1)) >> runForLoop1 newval var e prog
                                        _ -> throwExeError EnvError

runForLoop2 :: LanVal -> String -> [LanVal] -> Program -> Exe LanVal
runForLoop2 lastval _ [] _ = return lastval
runForLoop2 _ var (l:ls) prog = do
                                        setVarExe var l
                                        newval <- evalProg prog
                                        runForLoop2 newval var ls prog

runWhileLoop :: LanVal -> LanExpr -> Program -> Exe LanVal
runWhileLoop lastval cond body = do
                            res <- evalExpr cond
                            case res of
                                Bool b -> if not b
                                        then return lastval
                                        else do
                                                newval <- evalProg body
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
                                                            Int e -> if s>e 
                                                                    then throwExeError $ LoopRange s e
                                                                    else do
                                                                            enterBlock
                                                                            bindVarExe var s'
                                                                            res <- runForLoop1 (Int 0) var e body
                                                                            exitBlock
                                                                            return res
                                                            _ -> throwExeError $ TypeMismatch "Int" (LitVal e')
                                                _ -> throwExeError $ TypeMismatch "Int" (LitVal s')
evalBlock (ForLoop2 var lsexpr body) = do
                                        ls <- evalExpr lsexpr
                                        case ls of
                                            List xs -> do
                                                        enterBlock
                                                        bindVarExe var (Int 0)
                                                        res <- runForLoop2 (Int 0) var xs body
                                                        exitBlock
                                                        return res
                                            _ -> throwExeError $ TypeMismatch "List" lsexpr
evalBlock (WhileLoop cond body) = do
                                    enterBlock
                                    res <- runWhileLoop (Int 0) cond body
                                    exitBlock
                                    return res
evalBlock (Expr expr) = evalExpr expr
                                            
-- Evaluate Program
evalProg :: Program -> Exe LanVal
evalProg [] = return $ Int 0
evalProg [block] = evalBlock block
evalProg (b:bs) = evalBlock b >> evalProg bs