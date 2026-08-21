module PrimitiveOps where

import LanDef
-- do not import from anything else

import Data.Map hiding (all, map)
import Control.Monad.Except

type BinOp = LanVal -> LanVal -> ThrowsError LanVal

primitiveOps :: Map String BinOp
primitiveOps = fromList [("||", lor),
                        ("&&", land),
                        ("==", leq),
                        ("!=", lneq),
                        ("<", lst),
                        (">", llt),
                        ("<=", lste),
                        (">=", llte),
                        ("+", ladd),
                        ("-", lsub),
                        ("*", lmul),
                        ("/", ldiv),
                        ("//", lintdiv),
                        ("%", lmod)]

lor (Bool x) (Bool y) = return $ Bool (x || y)
lor x (Bool _) = throwError $ TypeMismatch "Bool" (LitVal x)
lor _ y = throwError $ TypeMismatch "Bool" (LitVal y)

land (Bool x) (Bool y) = return $ Bool (x && y)
land x (Bool _) = throwError $ TypeMismatch "Bool" (LitVal x)
land _ y = throwError $ TypeMismatch "Bool" (LitVal y)

leq :: LanVal -> LanVal -> ThrowsError LanVal
leq (Bool x) (Bool y) = return $ Bool (x == y)
leq (Char x) (Char y) = return $ Bool (x == y)
leq (String x) (String y) = return $ Bool (x == y)
leq (Int x) (Int y) = return $ Bool (x == y)
leq (Float x) (Float y) = return $ Bool (x == y)
leq (List xs) (List ys) = if length xs /= length ys then return $ Bool False
                        else do
                                let zs = zip xs ys
                                zs' <- mapM (uncurry leq) zs
                                let bs = Prelude.map (\(Bool x) -> x) zs'
                                return $ Bool (and bs)
leq x y = throwError $ TypeMismatch (show $ getType x) (LitVal y)

lneq x y = do
            Bool res <- leq x y
            return $ Bool (not res)

lst (Char x) (Char y) = return $ Bool (x<y)
lst (String x) (String y) = return $ Bool (x<y)
lst (Int x) (Int y) = return $ Bool (x<y)
lst (Float x) (Float y) = return $ Bool (x<y)
lst (Bool x) _ = throwError $ TypeMismatch "Ord" (LitVal $ Bool x)
lst _ (Bool y) = throwError $ TypeMismatch "Ord" (LitVal $ Bool y)
lst (List xs) _ = throwError $ TypeMismatch "Ord" (LitVal $ List xs)
lst _ (List ys) = throwError $ TypeMismatch "Ord" (LitVal $ List ys)
lst x@(Func _ _ _ _) _ = throwError $ TypeMismatch "Ord" (LitVal x)
lst _ y@(Func _ _ _ _) = throwError $ TypeMismatch "Ord" (LitVal y)
lst x y = throwError $ TypeMismatch (show $ getType x) (LitVal y)

lste x y = do
            Bool res1 <- lst x y
            Bool res2 <- leq x y
            return $ Bool (res1 || res2)

llt x y = do
            Bool res <- lste x y
            return $ Bool (not res)

llte x y = do
            Bool res1 <- llt x y
            Bool res2 <- leq x y
            return $ Bool (res1 || res2)

lintop :: (forall n. Num n => n -> n -> n) -> LanVal -> LanVal -> ThrowsError LanVal
lintop op (Int x) (Int y) = return $ Int (op x y)
lintop op (Float x) (Float y) = return $ Float (op x y)
lintop op (Int x) (Float y) = return $ Float (op (fromIntegral x) y)
lintop op (Float x) (Int y) = return $ Float (op x (fromIntegral y))
lintop op (Int x) y = throwError $ TypeMismatch "Num" (LitVal y)
lintop op (Float x) y = throwError $ TypeMismatch "Num" (LitVal y)
lintop op x _ = throwError $ TypeMismatch "Num" (LitVal x)

ladd = lintop (+)

lsub = lintop (-)

lmul = lintop (*)

matherror = MathError "Division by zero"
ldiv (Int x) (Int y) = if y==0 then throwError matherror else return $ Float (fromIntegral x / fromIntegral y)
ldiv (Int x) (Float y) = if y==0 then throwError matherror else return $ Float (fromIntegral x / y)
ldiv (Float x) (Int y) = if y==0 then throwError matherror else return $ Float (x / fromIntegral y)
ldiv (Float x) (Float y) = if y==0 then throwError matherror else return $ Float (x/y)
ldiv (Int x) y = throwError $ TypeMismatch "Num" (LitVal y)
ldiv (Float x) y = throwError $ TypeMismatch "Num" (LitVal y)
ldiv x _ = throwError $ TypeMismatch "Num" (LitVal x)

lintdiv (Int x) (Int y) = if y==0 then throwError matherror else return $ Int (x `div` y)
lintdiv (Int x) y = throwError $ TypeMismatch "Int" (LitVal y)
lintdiv x _ = throwError $ TypeMismatch "Int" (LitVal x)

lmod (Int x) (Int y) = if y==0 then throwError matherror else return $ Int (x `mod` y)
lmod (Int x) y = throwError $ TypeMismatch "Int" (LitVal y)
lmod x _ = throwError $ TypeMismatch "Int" (LitVal x)

instance MonadFail ThrowsError where
    fail :: String -> ThrowsError a
    fail str = throwError $ Default str