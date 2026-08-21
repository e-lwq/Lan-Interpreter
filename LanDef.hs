module LanDef where

-- Base File, do not import from anything else except helper functions
import Helper

import Control.Applicative
import Data.Map (Map)
import Control.Monad.Except
import Control.Monad.IO.Class
import Text.Parsec.Error (ParseError)

-- useful functions / data
lchars :: [Char]
lchars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_+=`~{}[]|/?,.<>:; "

prec0 :: [String]
prec0 = ["||"]

prec1 :: [String]
prec1 = ["&&"]

prec2 :: [String]
prec2 = ["==", "!=", "<=", ">=", "<", ">"]

prec3 :: [String]
prec3 = ["+", "-"]

prec4 :: [String]
prec4 = ["*", "//", "/", "%"]

convParam :: (String, LanType) -> String
convParam (str, tp) = str ++ ": " ++ show tp

addTab :: String -> String
addTab str = joinBy "\n" (map ("    " ++) (splitOn '\n' str))

showBody :: [Block] -> String
showBody body = addTab (joinBy "\n" (map show body))

-- LanType
data LanType = Any | Num | Ord | Concat | TBool | TChar | TString | TInt | TFloat | TList | TFunc deriving Eq

instance Show LanType where
    show Any = "Any"
    show Num = "Num"
    show Ord = "Ord"
    show Concat = "Concat"
    show TBool = "Bool"
    show TChar = "Char"
    show TString = "String"
    show TInt = "Int"
    show TFloat = "Float"
    show TList = "List"
    show TFunc = "Func"

getType :: LanVal -> LanType
getType val = case val of
                Bool _ -> TBool
                Char _ -> TChar
                String _ -> TString
                Int _ -> TInt
                Float _ -> TFloat
                List _ -> TList
                Func _ _ _ _ -> TFunc

-- LanVal
data LanVal = Bool Bool | Char Char | String String | Int Int | Float Float | List [LanVal] 
            | Func {name :: String, params :: [(String, LanType)], retType :: LanType, body :: [Block]}

instance Show LanVal where
    show (Bool True) = "True"
    show (Bool False) = "False"
    show (Char ch) = "'" ++ [ch] ++ "'"
    show (String str) = "\"" ++ str ++ "\""
    show (Int n) = show n
    show (Float n) = show n
    show (List ls) = "[" ++ joinBy ", "(map show ls) ++ "]"
    show (Func name params retType body) = name ++ "(" ++ joinBy ", " (map convParam params) ++ ") {...}"

-- LanExpr
data LanExpr = LitVal LanVal 
            | VarRef String 
            | FuncApp String [LanExpr] 
            | Op String LanExpr LanExpr

instance Show LanExpr where
    show (LitVal val) = show val
    show (VarRef var) = var
    show (FuncApp name args) = name ++ " $ (" ++ joinBy ", " (map show args) ++ ")"
    show (Op op e1 e2) = bracket (show e1) ++ op ++ bracket (show e2)

toBinTree :: LanExpr -> [(String, LanExpr)] -> LanExpr
toBinTree expr [] = expr
toBinTree expr ((op,expr'):es) = toBinTree (Op op expr expr') es

-- Block
data Block = DecVar String LanType LanExpr 
            | DecVar2 String LanExpr
            | SetVar String LanExpr 
            | DefFunc String [(String, LanType)] LanType [Block]
            | Cond LanExpr [Block] [Block] 
            | ForLoop1 String LanExpr LanExpr [Block] 
            | ForLoop2 String LanExpr [Block]
            | WhileLoop LanExpr [Block] 
            | Expr LanExpr

instance Show Block where
    show (DecVar name tp expr) = "var " ++ name ++ " : " ++ show tp ++ " = " ++ show expr
    show (DecVar2 name expr) = "var " ++ name ++ " = " ++ show expr
    show (SetVar name expr) = "set " ++ name ++ " = " ++ show expr 
    show (DefFunc name params retType body) = "def " ++ name ++ " (" ++ joinBy ", " (map convParam params) 
                                            ++ ") : " ++ show retType ++ " {\n" ++ showBody body ++ "\n}"
    show (Cond cond then_body else_body) = "if (" ++ show cond ++ ")\n"
                                        ++ "then {\n" ++ showBody then_body
                                        ++ "\n} else{\n" ++ showBody else_body ++ "\n}"
    show (ForLoop1 var start end body) = "for (" ++ var ++ " in range (" ++ show start ++ ", " ++ show end ++")){\n"
                                        ++ showBody body ++ "\n}"
    show (ForLoop2 var ls body) = "for (" ++ var ++ " in " ++ show ls ++ "){\n" 
                            ++ showBody body ++ "\n}"
    show (WhileLoop cond body) = "while (" ++ show cond ++ "){\n"
                                ++ showBody body ++ "\n}"
    show (Expr expr) = show expr

keywords :: [String]
keywords = ["var", "set", "def", "if", "for", "while"]

type Program = [Block]
showProg prog = joinBy "\n" (map show prog)

-- Lan Errors

data LanError = NumArgs Integer [LanExpr] 
                | TypeMismatch String LanExpr 
                | Parser ParseError 
                | IndOutRange LanExpr Int
                | VarNotFound String
                | VarDefTwice String
                | NotOp String
                | LoopRange Int Int
                | MathError String
                | Runtime String
                | Default String
                | EnvError
                | Empty

instance Show LanError where
    show (NumArgs n args) = "Expected " ++ show n ++ " arguments, but got: " ++ show args
    show (TypeMismatch tp expr) = "Expected type: " ++ show tp ++ ", but got: " ++ show expr
    show (Parser err) = "Parser error: " ++ show err
    show (IndOutRange ls n) = "Index out of range: " ++ show ls ++ " " ++ show n
    show (VarNotFound var) = "Variable not found: " ++ var
    show (VarDefTwice var) = "Variable defined twice: " ++ var
    show (NotOp op) = "Not a primitive operator: " ++ op
    show (LoopRange s e) = "Invalid range: (" ++ show s ++ ", " ++ show e ++ ")"
    show (MathError str) = "Math error: " ++ str
    show (Runtime str) = "Runtime error: " ++ str
    show (Default str) = "Error: " ++ str
    show EnvError = "Environment error"
    show Empty = "Empty error"

type ThrowsError = Either LanError -- ThrowsError type = Left error / Right (value :: type)

type IOThrowsError a = IO (ThrowsError a)

--throwError :: LanError -> ThrowsError a
--throwError err = Left err

extractVal :: Either a b -> b
extractVal (Right x) = x

type Env = [Map String LanVal] -- X use LanExpr, because I will be doing eager evaluation
-- implement as stack, where top of stack = head of Env

newtype Exe a = Exe (Env -> IOThrowsError (a,Env))

throwExeError :: LanError -> Exe a
throwExeError err = Exe (\env -> return $ throwError err)

instance Functor Exe where
    fmap :: (a -> b) -> Exe a -> Exe b
    fmap f (Exe exe) = Exe (\env -> do
                                        r <- exe env
                                        case r of
                                            Left err -> return $ Left err
                                            Right (res,env') -> return $ Right (f res, env'))

instance Applicative Exe where
    pure :: a -> Exe a 
    pure x = Exe (\env -> return $ Right (x,env))

    (<*>) :: Exe (a->b) -> Exe a -> Exe b
    Exe exe1 <*> Exe exe2 = Exe (\env -> do
                                            r <- exe1 env
                                            case r of
                                                Left err -> return $ Left err
                                                Right (f,env') -> do
                                                                    r' <- exe2 env' 
                                                                    case r' of
                                                                        Left err -> return $ Left err
                                                                        Right (x,env'') -> return $ Right (f x, env''))

instance Monad Exe where
    return :: a -> Exe a
    return = pure

    (>>=) :: Exe a -> (a -> Exe b) -> Exe b
    Exe exe >>= f = Exe (\env -> do
                                    r <- exe env
                                    case r of
                                        Left err -> return $ Left err
                                        Right (res,env') -> let Exe exe' = f res in
                                                            exe' env')

liftIO2Exe :: IOThrowsError a -> Exe a
liftIO2Exe m = Exe (\env -> do
                                v <- m
                                case v of
                                    Left err -> return $ throwError err
                                    Right res -> return $ return (res,env))

runExe :: Exe a -> Env -> IOThrowsError (a,Env)
runExe (Exe f) env = f env
