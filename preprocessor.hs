import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Expr
import Text.Parsec.Language

import Control.Monad.Trans.RWS
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
  | ESgn Expr
  | EAbs Expr
  | ESqr Expr
  | EDiffAbs Expr Expr
  | EAssign Expr Expr
  deriving Show

data Instruction
  = ISet String String
  | IAbs String String
  | INeg String String
  | ISgn String String
  | ISub String String String
  | IAdd String String String
  | IMul String String String
  | IMulI String String Int
  | ISqr String String
  deriving Show

temporaries = [ "t" ++ show i | i <- [0..] ]

isTemporary ('t':_) = True
isTemporary _ = False

allocate = do
  t:ts <- get
  put ts
  return t

deallocate t = do
  if isTemporary t
    then do
      ts <- get
      put (t:ts)
    else do
      return ()

instruction (ISet a b) = tell (["mpfr_set(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (IAbs a b) = tell (["mpfr_abs(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (INeg a b) = tell (["mpfr_neg(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ISub a b c) = tell (["mpfr_sub(", a, ",", b, ",", c, ",MPFR_RNDN);\n"], [a, b, c])
instruction (IAdd a b c) = tell (["mpfr_add(", a, ",", b, ",", c, ",MPFR_RNDN);\n"], [a, b, c])
instruction (IMulI a b c) = tell (["mpfr_mul_ui(", a, ",", b, ",", show (abs c), ",MPFR_RNDN);\n"] ++ if c < 0 then ["mpfr_neg(", a, ",", a, ",MPFR_RNDN);"] else [], [a, b])
instruction (IMul a b c) = tell (["mpfr_mul(", a, ",", b, ",", c, ",MPFR_RNDN);\n"], [a, b, c])
instruction (ISqr a b) = tell (["mpfr_mul(", a, ",", b, ",", b, ",MPFR_RNDN);\n"], [a, b])

compile (EAssign (EVar v) a) = do
  Just u <- compile a
  instruction (ISet v u)
  deallocate u
  return Nothing

compile (EVar v) = do
  return (Just v)

compile (EAbs a) = do
  Just u <- compile a
  v <- allocate
  instruction (IAbs v u)
  deallocate u
  return (Just v)

compile (ENeg a) = do
  Just u <- compile a
  v <- allocate
  instruction (INeg v u)
  deallocate u
  return (Just v)

compile (ESgn a) = do
  Just u <- compile a
  v <- allocate
  instruction (ISgn v u)
  deallocate u
  return (Just v)

compile (ESub a b) = do
  Just u <- compile a
  Just v <- compile b
  w <- allocate
  instruction (ISub w u v)
  deallocate u
  deallocate v
  return (Just w)

compile (EAdd a b) = do
  Just u <- compile a
  Just v <- compile b
  w <- allocate
  instruction (IAdd w u v)
  deallocate u
  deallocate v
  return (Just w)

compile (EMul (EInt a) b) = do
  Just v <- compile b
  w <- allocate
  instruction (IMulI w v a)
  deallocate v
  return (Just w)

compile (EMul a (EInt b)) = do
  Just u <- compile a
  w <- allocate
  instruction (IMulI w u b)
  deallocate u
  return (Just w)

compile (EMul (ENeg (EInt a)) b) = do
  Just v <- compile b
  w <- allocate
  instruction (IMulI w v (negate a))
  deallocate v
  return (Just w)

compile (EMul a (ENeg (EInt b))) = do
  Just u <- compile a
  w <- allocate
  instruction (IMulI w u (negate b))
  deallocate u
  return (Just w)

compile (EMul a b) = do
  Just u <- compile a
  Just v <- compile b
  w <- allocate
  instruction (IMul w u v)
  deallocate u
  deallocate v
  return (Just w)

compile (ESqr a) = do
  Just u <- compile a
  v <- allocate
  instruction (ISqr v u)
  deallocate u
  return (Just v)

compile x = error (show x)

init2 = concatMap (\t -> "mpfr_t " ++ t ++ "; mpfr_init2(" ++ t ++ ", bits);\n")
clear = concatMap (\t -> "mpfr_clear(" ++ t ++ ");\n")

runCompile es = case evalRWS (mapM_ compile es) () temporaries of
  ((), (is, vs)) ->
    let ts = filter isTemporary (nub vs)
    in  init2 ts ++ loopStart ++ concat is ++ loopEnd ++ clear ts

loopStart = "for (i = 0; i < nMaxIter && !m_bStop; i++) { DLOOP\n"
loopEnd   = "LOOP }\n"

vars (EInt _) = []
vars (EVar a) = [a]
vars (EPow a b) = vars a ++ vars b
vars (EMul a b) = vars a ++ vars b
vars (EAdd a b) = vars a ++ vars b
vars (ESub a b) = vars a ++ vars b
vars (ENeg a) = vars a
vars (ESgn a) = vars a
vars (EAbs a) = vars a
vars (ESqr a) = vars a
vars (EDiffAbs a b) = vars a ++ vars b
vars (EAssign a b) = vars a ++ vars b

interpret _ (EInt n) = show n
interpret _ (EVar v) = v
interpret t (EPow a b) = "(" ++ interpret t a ++ "^" ++ interpret t b ++ ")"
interpret t (EMul a@(ENeg (EInt _)) b) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t (EMul a b@(ENeg (EInt _))) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t (EMul a@(EInt _) b) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t (EMul a b@(EInt _)) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t (EMul a b) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t (EAdd a b@(ENeg (EInt _))) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t (EAdd a b@(EInt _)) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t (EAdd a@(ENeg (EInt _)) b) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t (EAdd a@(EInt _) b) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t (EAdd a b) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t (ESub a b@(ENeg (EInt _))) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t (ESub a b@(EInt _)) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t (ESub a@(ENeg (EInt _)) b) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t (ESub a@(EInt _) b) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t (ESub a b) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t (ENeg (EInt v)) = interpret t (EInt (negate v))
interpret t (ENeg a) = "-(" ++ interpret t a ++ ")"
interpret t (ESgn a) = "sgn(" ++ interpret t a ++ ")"
interpret t (EAbs a) = "abs(" ++ interpret t a ++ ")"
interpret t (ESqr a) = "sqr(" ++ interpret t a ++ ")"
interpret t (EDiffAbs a b) = "diffabs(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAssign (EVar v) a) = v ++ "=" ++ interpret t a ++ ";"

prepare "d" vs = unlines . concat $
  [ [ "const T Xr2 = Xr * Xr;" | "Xr2" `elem` vs ]
  , [ "const T Xi2 = Xi * Xi;" | "Xi2" `elem` vs ]
  , [ "const T xr2 = xr * xr;" | "xr2" `elem` vs ]
  , [ "const T xi2 = xi * xi;" | "xi2" `elem` vs ]
  ]
prepare "dc" vs = unlines . concat $
  [ [ "const complex<T> X2 = X * X;" | "X2" `elem` vs ]
  , [ "const complex<T> x2 = x * x;" | "x2" `elem` vs ]
  ]

def :: GenLanguageDef String () Identity
def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf ops
              , opLetter = oneOf ops
              , reservedOpNames = map (:[]) ops
              , reservedNames = ["sqr", "sgn", "abs", "diffabs"]
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
       <|> (m_reserved "sqr" >> ESqr <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "sgn" >> ESgn <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "abs" >> EAbs <$ string "(" <*> exprparser <* string ")")
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
interpretCL (Block "rd" s) = unlines . (++ [""]) . map (++ " \\") . ("#define DLOOP" :) . lines $ s
interpretCL (Block "rr" s) = case parse blockp "" $ filter (not . isSpace) s of
  Right es -> runCompile es ++ "#undef DLOOP\n"
  Left e -> error (show e ++ " : " ++ show s)
interpretCL (Block "rc" s) = "assert(! \"implemented yet\");\n"
interpretCL (Block t s) = case parse blockp "" $ filter (not . isSpace) s of
  Right es -> "{" ++ prepare t (nub $ concatMap vars es) ++ unlines (map (interpret (t ++ "_")) es) ++ "}"
  Left e -> error (show e ++ " : " ++ show s)

main = interact parseCL
