import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language

import Control.Monad.Identity (Identity)

import Data.Char (isSpace)

data Expr
  = EInt Int
  | EVar String
  | EPow Expr Expr
  | EMul Expr Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | ENeg Expr
  | EAbs Expr
  | EDiffAbs Expr Expr
  | EAssign Expr Expr

interpret _ (EInt n) = show n
interpret _ (EVar v) = v
interpret t (EPow a b) = t ++ "pow(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a@(ENeg (EInt _)) b) = t ++ "imul(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a b@(ENeg (EInt _))) = t ++ "muli(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a@(EInt _) b) = t ++ "imul(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a b@(EInt _)) = t ++ "muli(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a b) = t ++ "mul(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAdd a b) = t ++ "add(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ESub a b) = t ++ "sub(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ENeg (EInt v)) = interpret t (EInt (negate v))
interpret t (ENeg a) = t ++ "neg(" ++ interpret t a ++ ")"
interpret t (EAbs a) = t ++ "abs(" ++ interpret t a ++ ")"
interpret t (EDiffAbs a b) = t ++ "diffabs(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAssign (EVar v) a) = v ++ "=" ++ interpret t a ++ ";"

def :: GenLanguageDef String () Identity
def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf ops
              , opLetter = oneOf ops
              , reservedOpNames = map (:[]) ops
              , reservedNames = ["abs", "diffabs"]
              }
  where ops = "=+-*^"

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = op
           , reserved = m_reserved
           , integer = m_integer } = makeTokenParser def

exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (op "-" >> return ENeg)]
        , [Infix (op "^" >> return EPow) AssocLeft]
        , [Infix (op "*" >> return EMul) AssocLeft]
        , [Infix (op "+" >> return EAdd) AssocLeft
          ,Infix (op "-" >> return ESub) AssocLeft]
        , [Infix (op "=" >> return EAssign) AssocLeft]
        ]
term = m_parens exprparser
       <|> (EVar <$> m_identifier)
       <|> (EInt . fromIntegral <$> m_integer)
       <|> (char '-' >> EInt . negate . fromIntegral <$> m_integer)
       <|> (m_reserved "abs" >> EAbs <$ string "(" *> exprparser <* string ")")
       <|> (m_reserved "diffabs" >> EDiffAbs <$ string "(" *> exprparser <* string "," *> exprparser <* string ")")

data CL
  = Context String
  | Block String String

context = Context <$> many1 (noneOf "@")
block :: ParsecT String () Identity CL
block = do
  char '@'
  t <- many (noneOf " ")
  many (char ' ')
  char '{'
  s <- many (noneOf "}")
  char '}'
  pure (Block t s)

blockp = many (exprparser <* string ";") <* eof

parseCL s = case parse (many (block <|> context) <* eof) "" s of
  Right cl -> concatMap interpretCL cl
  Left e -> error (show e)

interpretCL (Context s) = s
interpretCL (Block t s) = case parse blockp "" $ filter (not . isSpace) s of
  Right es -> concatMap (interpret (t ++ "_")) es
  Left e -> error (show e ++ " : " ++ show s)

main = interact parseCL
