module Parser where

import Helper
import LanDef

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (token, space, spaces, choice)


-- useful functions
underscore :: Parser Char
underscore = char '_'

comma :: Parser ()
comma = token (char ',') >> return ()

space :: Parser Char
space = char ' '

spaces :: Parser ()
spaces = many space >> return ()

nextline :: Parser ()
nextline = spaces >> char '\n' >> spaces

token :: Parser a -> Parser a
token p = spaces >> p >>= \res -> spaces >> return res

token1 :: Parser a -> Parser a
token1 p = spaces >> p

token2 :: Parser a -> Parser a
token2 p = p >>= \res -> spaces >> return res

-- parse Lan values
parseBool :: Parser LanVal
parseBool = (string "True" </> string "False") >>= \b ->
                return $ Bool (if b=="True" then True else False)

parseChar :: Parser LanVal
parseChar = do
                char '\''
                c <- oneOf lchars
                char '\''
                return (Char c)

parseString :: Parser LanVal
parseString = do
                char '"'
                str <- many (oneOf lchars)
                token2 (char '"')
                return (String str)

parseInt :: Parser LanVal
parseInt = (do
            char '-'
            n <- many1 digit
            return $ Int $ (-1)*(read n)) </>
            (do
                n <- many1 digit
                return (Int (read n)))

parseFloat :: Parser LanVal
parseFloat = (do
                char '-'
                n <- many1 digit
                char '.'
                d <- many1 digit
                let frac :: Float = read d / fromIntegral (10^length d)
                return $ Float $ (-1)*(read n + frac)) </>
            (do
                n <- many1 digit
                char '.'
                d <- many1 digit
                let frac :: Float = read d / fromIntegral (10^length d)
                return $ Float (read n + frac))

parseList :: Parser LanVal
parseList = do
                char '['
                spaces
                ls <- sepBy parseVal (token (char ','))
                spaces
                char ']'
                return $ List ls

parseVal :: Parser LanVal
parseVal = parseBool </> parseChar </> parseString </> parseFloat </> parseInt </> parseList

-- parse Lan exprs
parseLitVal :: Parser LanExpr
parseLitVal = do
                val <- token parseVal
                return $ LitVal val

parseName :: Parser String
parseName = token $ do
                x <- oneOf "abcdefghijklmnopqrstuvwxyz"
                xs <- many (oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
                return (x:xs)

parseVarRef :: Parser LanExpr
parseVarRef = do
                name <- parseName 
                return $ VarRef name

parseFuncApp :: Parser LanExpr
parseFuncApp = do
                name <- parseName
                token (char '(')
                args <- sepBy parseExpr comma
                token (char ')')
                return $ FuncApp name args

parseAtom :: Parser LanExpr
parseAtom = parseLitVal </> parseFuncApp </> parseVarRef 
            </> do
                    token (char '(')
                    expr <- parseExpr
                    token (char ')')
                    return expr

parseFactor :: Parser LanExpr
parseFactor = do
                atom <- parseAtom
                let {
                    p4 :: Parser String = token $ choice $ toStrParser prec4;
                    ext :: Parser (String, LanExpr) = do
                                                        op <- p4
                                                        a <- parseAtom
                                                        return (op, a)
                }
                atoms <- many ext
                return $ toBinTree atom atoms
                
parseTerm :: Parser LanExpr
parseTerm = do
                factor <- parseFactor
                let {
                    p3 :: Parser String = token $ choice $ toStrParser prec3;
                    ext :: Parser (String, LanExpr) = do
                                                        op <- p3
                                                        a <- parseFactor
                                                        return (op, a)
                }
                factors <- many ext
                return $ toBinTree factor factors

parseComp :: Parser LanExpr
parseComp = do
                term <- parseTerm
                let {
                    p2 :: Parser String = token $ choice $ toStrParser prec2;
                    ext :: Parser (String, LanExpr) = do
                                                        op <- p2
                                                        a <- parseTerm
                                                        return (op, a)
                }
                terms <- many ext
                return $ toBinTree term terms

parseAndExpr :: Parser LanExpr
parseAndExpr = do
                    comp <- parseComp
                    let {
                        p1 :: Parser String = token $ choice $ toStrParser prec1;
                        ext :: Parser (String, LanExpr) = do
                                                            op <- p1
                                                            a <- parseComp
                                                            return (op, a)
                    }
                    comps <- many ext
                    return $ toBinTree comp comps

parseExpr :: Parser LanExpr
parseExpr = do
                andexpr <- parseAndExpr
                let {
                    p0 :: Parser String = token $ choice $ toStrParser prec0;
                    ext :: Parser (String, LanExpr) = do
                                                        op <- p0
                                                        a <- parseAndExpr
                                                        return (op, a)
                }
                andexprs <- many ext
                return $ toBinTree andexpr andexprs

-- parse Lan block
parseType :: Parser LanType
parseType = do
                t <- token $ choice $ toStrParser ["Bool", "Char", "String", "Int", "Float", "List"]
                return $ case t of
                    "Bool" -> TBool
                    "Char" -> TChar
                    "String" -> TString
                    "Int" -> TInt
                    "Float" -> TFloat
                    "List" -> TList

parseDecVar :: Parser Block
parseDecVar = do
                name <- parseName
                token $ char ':'
                t <- parseType
                token $ char '='
                val <- parseExpr
                (token $ char ';') </> (token $ char '\n')
                return $ DecVar name t val

parseSetVar :: Parser Block
parseSetVar = do
                name <- parseName
                token $ char '='
                expr <- parseExpr
                (token $ char ';') </> (token $ char '\n')
                return $ SetVar name expr

parseDefFunc :: Parser Block
parseDefFunc = do
                name <- parseName
                token $ char '('
                let {
                    param :: Parser (String, LanType) = do
                        pn <- parseName
                        token $ char ':'
                        tn <- parseType
                        return (pn, tn)
                }
                params <- sepBy param comma
                token $ char ')'
                token $ char ':'
                retType <- parseType
                token $ char '{'
                blocks <- many parseBlock -- ??? parseBlock includes parsing nextline
                token $ char '}'
                return $ DefFunc name params retType blocks

parseCond :: Parser Block
parseCond = do
                token $ char '('
                cond <- parseExpr
                token $ char ')'
                many nextline
                token $ string "then"
                token $ char '{'
                then_body <- many parseBlock
                token $ char '}'
                many nextline
                token $ string "else"
                many nextline
                token $ char '{'
                else_body <- many parseBlock
                token $ char '}'
                return $ Cond cond then_body else_body

parseForLoop :: Parser Block
parseForLoop = (do
                    token $ char '('
                    var <- parseName
                    token $ string "in"
                    return var) >>=
                (\name -> ((do
                    token $ string "range"
                    token $ char '('
                    start <- parseExpr
                    comma
                    end <- parseExpr
                    token $ char ')'
                    token $ char ')'
                    token $ char '{'
                    loop_body <- many parseBlock
                    token $ char '}'
                    return $ ForLoop1 name start end loop_body)
                </> (do
                        ls <- parseExpr
                        token $ char ')'
                        token $ char '{'
                        loop_body <- many parseBlock
                        token $ char '}'
                        return $ ForLoop2 name ls loop_body)))

parseWhileLoop :: Parser Block
parseWhileLoop = do
                    token $ char '('
                    cond <- parseExpr
                    token $ char ')'
                    token $ char '{'
                    loop_body <- many parseBlock
                    token $ char '}'
                    return $ WhileLoop cond loop_body

parseBlock' :: Parser Block
parseBlock' = (do
                keyword <- token $ choice $ toStrParser keywords
                case keyword of
                    "var" -> parseDecVar
                    "set" -> parseSetVar
                    "def" -> parseDefFunc
                    "if" -> parseCond
                    "for" -> parseForLoop
                    "while" -> parseWhileLoop)
            </> (do
                    expr <- parseExpr
                    (token $ char ';') </> (token $ char '\n')
                    return $ Expr expr)

parseBlock :: Parser Block
parseBlock = do
                many nextline
                block <- parseBlock'
                many nextline
                return block