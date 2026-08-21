module Helper where

import Text.ParserCombinators.Parsec hiding (choice)

-- Most Basic file, do not import from anything else

infixr 3 </>
(</>) :: Parser a -> Parser a -> Parser a 
pa </> pb = try pa <|> pb

joinBy :: String -> [String] -> String
joinBy _ [] = ""
joinBy _ [x] = x
joinBy j (x:xs) = x ++ j ++ joinBy j xs

choice :: [Parser a] -> Parser a
choice [p] = p
choice (p:ps) = p </> choice ps

toStrParser :: [String] -> [Parser String]
toStrParser = map string

bracket :: String -> String
bracket str = "(" ++ str ++ ")"

splitOn :: Char -> String -> [String]
splitOn _ [] = [[]]
splitOn c (x:xs) | c==x = []:x':xs'
                    | otherwise = (x:x'):xs'
                where (x':xs') = splitOn c xs

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil p (x:xs) | p x = (x:xs)
                | otherwise = dropUntil p xs

isLeft :: Either a b -> Bool
isLeft (Left x) = True
isLeft _ = False