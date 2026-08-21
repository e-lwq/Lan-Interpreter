module Evaluator where

import Helper
import LanDef
import PrimitiveOps

import Data.Map (Map, lookup, fromList)
import Prelude hiding (lookup)
import Control.Monad.Except

-- Evaluate exprs, SHOULD NOT ALTER ENVIRONMENT
evalExpr :: Env -> LanExpr -> ThrowsError LanVal
evalExpr _ (LitVal val) = return val
evalExpr env (VarRef varname) = case lookupVar env varname of
                                    Nothing -> throwError $ VarNotFound varname
                                    Just val -> return val
evalExpr env (FuncApp funcname args) = case lookupVar env funcname of
                                        Nothing -> throwError $ VarNotFound funcname
                                        Just func -> apply env func args
evalExpr env (Op op e1 e2) = do
                                v1 <- evalExpr env e1
                                v2 <- evalExpr env e2
                                case lookup op primitiveOps of
                                    Nothing -> throwError $ NotOp op
                                    Just f -> f v1 v2

lookupVar :: Env -> String -> Maybe LanVal
lookupVar [] varname = Nothing
lookupVar (env:es) varname = case lookup varname env of
                                Nothing -> lookupVar es varname
                                Just val -> Just val

apply :: Env -> LanVal -> [LanExpr] -> ThrowsError LanVal
apply env (func@(Func name params retType body)) args = do
                                                            eval_args <- checkFuncApp env func args
                                                            let env' = bindVars (zip (map fst params) eval_args) env
                                                            res <- evalProg env' body
                                                            if getType res == retType 
                                                            then return res
                                                            else throwError $ TypeMismatch ("Return type " ++ show retType) (LitVal res)

checkFuncApp :: Env -> LanVal -> [LanExpr] -> ThrowsError [LanVal]
checkFuncApp env (Func name params retType body) args | lp /= la = throwError $ NumArgs lp args
                                                    | isLeft eval_args = eval_args
                                                    | otherwise = let {vals = extractVal eval_args;
                                                                    vs = zip args vals;
                                                                    ls = zip params vs; -- ls = [((param name, param type),(arg expr, arg val))]
                                                                    ls' = dropUntil (\((pn,pt),(ae,av))-> pt /= getType av) ls;} in
                                                                    if null ls' then return vals
                                                                    else throwError $ TypeMismatch (show $ snd $ fst $ head ls) (fst $ snd $ head ls)
    where
        lp = fromIntegral $ length params
        la = fromIntegral $ length args
        eval_args = mapM (evalExpr env) args

bindVars :: [(String, LanVal)] -> Env -> Env
bindVars vars env = fromList vars : env


-- Evaluate Block
exitBlock :: Exe ()
exitBlock = Exe \env -> if null env then throwError EnvError else return ((), tail env)

enterBlock :: Exe ()
enterBlock = Exe \env -> return ((),(fromList []):env)

evalExpr2Exe :: LanExpr -> Exe LanVal
evalExpr2Exe expr = Exe \env -> case evalExpr env expr of
                                    Left err -> throwError err
                                    Right val -> return (val,env)

bindVarExe :: String -> LanVal -> Exe LanVal
bindVarExe varname val = Exe \env -> if null env then throwExeError EnvError
                                else case lookup varname (head env) of
                                        Just _ -> throwError $ VarDefTwice varname
                                        Nothing -> return (val, insert varname val (head env) : tail env)

lookupVarExe :: String -> Exe LanVal
lookupVarExe varname = Exe \env -> case lookupVar env varname of
                                    Nothing -> throwError $ VarNotFound varname
                                    Just val -> return (val,env)

setVarExe :: String -> LanVal -> Exe LanVal
setVarExe varname val = Exe \env -> case setVar env varname val of
                                        Left err -> throwError err
                                        Right res -> return (val,res)

setVar :: Env -> String -> LanVal -> ThrowsError Env
setVar [] varname _ = throwError $ VarNotFound varname
setVar (env:es) varname val = case lookup varname env of
                                Nothing -> (env:) <$> setVar es varname val
                                Just _ -> return ((insert varname val env):es)

defFunc :: String -> [(String, LanType)] -> LanType -> [Block] -> Exe LanVal
defFunc funcname params retType body = Exe \env -> case lookupVar env funcname of
                                                    Just _ -> throwError $ VarDefTwice funcname
                                                    Nothing -> if null env then throwError EnvError
                                                                else return (f,insert funcname f (head env) : tail env)
                                                    where f = Func funcname params retType body

runForLoop1 :: String -> Int -> Program -> Exe LanVal
runForLoop1 var e prog = do
                            val <- lookupVarExe var
                            case val of
                                Int v -> if v==e then return (Int 0)
                                        else if v==e-1 then evalProg prog
                                        else setVarExe var (Int (v+1)) >> runForLoop1 var e prog
                                _ -> throwExeError EnvError

evalBlock :: Block -> Exe LanVal
evalBlock (DecVar varname vartype expr) = do
                                            val <- evalExpr2Exe expr
                                            if getType val /= vartype
                                            then throwExeError $ TypeMismatch (show vartype) expr
                                            else bindVarExe varname val
evalBlock (SetVar varname expr) = do
                                    val <- evalExpr2Exe expr
                                    setVarExe varname val
evalBlock (DefFunc funcname params retType body) = do
                                                    val <- defFunc funcname params retType body
                                                    return val
evalBlock (Cond cond then_body else_body) = do
                                                res <- evalExpr2Exe cond
                                                case res of
                                                    Bool b -> do
                                                                enterBlock
                                                                val <- if b then evalProg then_body else evalProg else_body
                                                                exitBlock
                                                                return val
                                                    _ -> throwExeError $ TypeMismatch "Bool" cond
evalBlock (ForLoop1 var start end body) = do
                                            s' <- evalExpr2Exe start
                                            e' <- evalexpr2Exe end
                                            case s' of
                                                Int s -> case e' of
                                                            Int e -> if s>e 
                                                                    then throwExeError $ LoopRange s e
                                                                    else do
                                                                            enterBlock
                                                                            bindVarExe var s'
                                                                            res <- runForLoop1 var e body
                                                                            exitBlock
                                                                            return res
                                                            _ -> throwExeError $ TypeMismatch "Int" (LitVal e')
                                                _ -> throwExeError $ TypeMismatch "Int" (LitVal s')

                                            
-- Evaluate Program
evalProg :: Program -> Exe LanVal
evalProg [] = return $ Int 0
evalProg [block] = evalBlock block
evalProg (b:bs) = evalBlock b >> evalProg bs