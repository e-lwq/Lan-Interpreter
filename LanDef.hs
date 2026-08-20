module LanDef where

-- Base File, do not import from anything else except helper functions
import Helper


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

data LanType = TBool | TChar | TString | TInt | TFloat | TList | TFunc deriving (Eq, Show)

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

data LanExpr = LitVal LanVal | VarRef String | FuncApp String [LanExpr] | Op String LanExpr LanExpr

instance Show LanExpr where
    show (LitVal val) = show val
    show (VarRef var) = var
    show (FuncApp name args) = name ++ " $ (" ++ joinBy ", " (map show args) ++ ")"
    show (Op op e1 e2) = bracket (show e1) ++ op ++ bracket (show e2)

toBinTree :: LanExpr -> [(String, LanExpr)] -> LanExpr
toBinTree expr [] = expr
toBinTree expr ((op,expr'):es) = toBinTree (Op op expr expr') es

data Block = DecVar String LanType LanExpr | SetVar String LanExpr | DefFunc String [(String, LanType)] LanType [Block]
            | Cond LanExpr [Block] [Block] | ForLoop1 String LanExpr LanExpr [Block] | ForLoop2 String LanExpr [Block]
            | WhileLoop LanExpr [Block] | Expr LanExpr

instance Show Block where
    show (DecVar name tp expr) = "var " ++ name ++ " : " ++ show tp ++ " = " ++ show expr
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