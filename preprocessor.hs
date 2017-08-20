import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language

import Control.Monad.Identity (Identity)

import Data.Char (isSpace)
import Data.List (nub)

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

vars (EInt _) = []
vars (EVar a) = [a]
vars (EPow a b) = vars a ++ vars b
vars (EMul a b) = vars a ++ vars b
vars (EAdd a b) = vars a ++ vars b
vars (ESub a b) = vars a ++ vars b
vars (ENeg a) = vars a
vars (EAbs a) = vars a
vars (EDiffAbs a b) = vars a ++ vars b
vars (EAssign a b) = vars a ++ vars b

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

prepare "d" vs = unlines . concat $
  [ [ "const double Xr2 = Xr * Xr;" | "Xr2" `elem` vs ]
  , [ "const double Xi2 = Xi * Xi;" | "Xi2" `elem` vs ]
  , [ "const double xr2 = xr * xr;" | "xr2" `elem` vs ]
  , [ "const double xi2 = xi * xi;" | "xi2" `elem` vs ]
  ]
prepare "ld" vs = unlines . concat $
  [ [ "const long double Xr2 = Xr * Xr;" | "Xr2" `elem` vs ]
  , [ "const long double Xi2 = Xi * Xi;" | "Xi2" `elem` vs ]
  , [ "const long double xr2 = xr * xr;" | "xr2" `elem` vs ]
  , [ "const long double xi2 = xi * xi;" | "xi2" `elem` vs ]
  ]
prepare "fe" vs = unlines . concat $
  [ [ "const floatexp Xr2 = fe_mul(Xr, Xr);" | "Xr2" `elem` vs ]
  , [ "const floatexp Xi2 = fe_mul(Xi, Xi);" | "Xi2" `elem` vs ]
  , [ "const floatexp xr2 = fe_mul(xr, xr);" | "xr2" `elem` vs ]
  , [ "const floatexp xi2 = fe_mul(xi, xi);" | "xi2" `elem` vs ]
  ]
prepare "dc" vs = unlines . concat $
  [ [ "const dcomplex X2 = dc_mul(X, X);" | "X2" `elem` vs ]
  , [ "const dcomplex x2 = dc_mul(x, x);" | "x2" `elem` vs ]
  ]
prepare "ldc" vs = unlines . concat $
  [ [ "const ldcomplex X2 = ldc_mul(X, X);" | "X2" `elem` vs ]
  , [ "const ldcomplex x2 = ldc_mul(x, x);" | "x2" `elem` vs ]
  ]
prepare "fec" vs = unlines . concat $
  [ [ "const fecomplex X2 = fec_mul(X, X);" | "X2" `elem` vs ]
  , [ "const fecomplex x2 = fec_mul(x, x);" | "x2" `elem` vs ]
  ]

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
       <|> (m_reserved "diffabs" >> EDiffAbs <$ string "(" <*> exprparser <* string "," <*> exprparser <* string ")")

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
  Right es -> "{" ++ prepare t (nub $ concatMap vars es) ++ unlines (map (interpret (t ++ "_")) es) ++ "}"
  Left e -> error (show e ++ " : " ++ show s)

main = interact parseCL
