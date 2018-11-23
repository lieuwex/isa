{-# LANGUAGE MultiWayIf #-}
module Parser(parseProgram) where

import Control.Monad
import Data.Char
import Text.Parsec
import Text.Parsec.Expr

import AST


type Parser a = Parsec String () a


parseProgram :: String -> String -> Either ParseError Program
parseProgram fname src = runParser (pProgram <* endInput) () fname src

pProgram :: Parser Program
pProgram = Program <$> many pDecl

pDecl :: Parser Decl
pDecl = do
    symbol "def"
    name <- pName
    symbol "="
    expr <- pExpr
    return $ Decl name expr

pExpr :: Parser Expr
pExpr = buildExpressionParser optable pSimple
  where
    pSimple = pLambda <|> pCallOrRef

    pLambda = do
        symbol "\\"
        name <- pName
        symbol "->"
        expr <- pExpr
        return $ ELambda name expr

    pCallOrRef = chainl1 pAtom (return ECall)

    pAtom = (ERef <$> pName) <|>
            (EInt <$> pInt) <|>
            -- (EBool <$> pBool) <|>
            between (symbol "(") (symbol ")") pExpr

    optable =
        [[binop "*", binop "/"],
         [binop "+", binop "-"],
         [binop ".<<", binop ".>>", binop ".>>>"],
         [binop "<", binop ">", binop "<=", binop ">=", binop "==", binop "!="],
         [binop "&&"],
         [binop "||"],
         [binop ">>"]]

    binop name =
        Infix (symbol name >> return (\e1 e2 -> ECall (ECall (ERef name) e1) e2))
              AssocLeft

pName :: Parser Name
pName = try $ do
    skipWhite
    c1 <- satisfy isInitWordChar
    cs <- many (satisfy isWordChar)
    let name = c1 : cs
    guard (not $ name `elem` keywords)
    return name
  where
    keywords = ["def"]

pInt :: Parser Int
pInt = try $ do
    skipWhite
    s1 <- liftM (maybe "" (const "-")) $ optionMaybe (string "-" <|> string "\8722")
    s2 <- many1 (satisfy isDigit)
    return $ read (s1 ++ s2)

-- pBool :: Parser Bool
-- pBool = (symbol "true" >> return True) <|> (symbol "false" >> return False)

pComment :: Parser ()
pComment = pLineComment <|> pBlockComment

pLineComment :: Parser ()
pLineComment = do
    void $ try $ string "--"
    void $ manyTill anyChar (char '\n')

pBlockComment :: Parser ()
pBlockComment = do
    void $ try $ string "{-"
    void $ manyTill anyChar (try (string "-}"))

symbol :: String -> Parser ()
symbol s = try $ do
    skipWhite
    void $ string s
    if | isWordChar (head s) -> notFollowedBy (satisfy isWordChar)
       | isSymbolChar (head s) -> notFollowedBy (satisfy isSymbolChar)
       | otherwise -> return ()

isInitWordChar :: Char -> Bool
isInitWordChar c = isAlpha c || c == '_'

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` ".+-*/&|=><"

endInput :: Parser ()
endInput = skipWhite >> eof

skipWhite :: Parser ()
skipWhite = void $ spaces `sepBy` pComment
