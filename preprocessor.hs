{-
Kalles Fraktaler 2
Copyright (C) 2013-2017 Karl Runmo
Copyright (C) 2017-2021 Claude Heiland-Allen

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
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
  | EDiv Expr Expr
  | EAdd Expr Expr
  | ESub Expr Expr
  | EGt Expr Expr
  | ELt Expr Expr
  | ENeg Expr
  | ESgn Expr
  | EAbs Expr
  | ESqr Expr
  | EExp Expr
  | ELog Expr
  | ELog1p Expr
  | ESin Expr
  | ECos Expr
  | ESinh Expr
  | ECosh Expr
  | ESqrt Expr
  | EDiffAbs Expr Expr
  | ESpecial_93_2 Expr Expr Expr Expr
  | EAssign Expr Expr
  | EIf Expr Expr
  | ETE Expr Expr
  deriving Show

data Instruction
  = ISet String String
  | IAbs String String
  | INeg String String
  | ISgn String String
  | ISub String String String
  | IISub String Int String
  | IIAdd String Int String
  | IAdd String String String
  | IMul String String String
  | IMulI String String Int
  | IDiv String String String
  | IDivI String String Int
  | IDivII String Int Int
  | ISqr String String
  | IExp String String
  | ILog String String
  | ILog1p String String
  | ISin String String
  | ICos String String
  | ISinh String String
  | ICosh String String
  | ISqrt String String
  | IIfTE String String String String
  deriving Show

temporaries = [ "t" ++ show i | i <- [0..] ]

isTemporary ('t':_) = True
isTemporary _ = False

allocate = do
  ~(t:ts) <- get
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
instruction (IIAdd a b c) = tell (["mpfr_add_si(", a, ",", c, ",", show b, ",MPFR_RNDN);\n"], [a, c])
instruction (IISub a b c) = tell (["mpfr_si_sub(", a, ",", show b, ",", c, ",MPFR_RNDN);\n"], [a, c])
instruction (IAdd a b c) = tell (["mpfr_add(", a, ",", b, ",", c, ",MPFR_RNDN);\n"], [a, b, c])
instruction (IMulI a b c) = tell (["mpfr_mul_ui(", a, ",", b, ",", show (abs c), ",MPFR_RNDN);\n"] ++ if c < 0 then ["mpfr_neg(", a, ",", a, ",MPFR_RNDN);"] else [], [a, b])
instruction (IMul a b c) | b == c = tell (["mpfr_sqr(", a, ",", b, ",MPFR_RNDN);\n"], [a, b, c])
                         | otherwise = tell (["mpfr_mul(", a, ",", b, ",", c, ",MPFR_RNDN);\n"], [a, b, c])
instruction (IDivI a b c) = tell (["mpfr_div_si(", a, ",", b, ",", show c, ",MPFR_RNDN);\n"], [a, b])
instruction (IDivII a b c) = tell (["mpfr_set_si(", a, ",", show b, ",MPFR_RNDN);\nmpfr_div_si(", a, ",", a, ",", show c, ",MPFR_RNDN);\n"], [a])
instruction (IDiv a b c) = tell (["mpfr_div(", a, ",", b, ",", c, ",MPFR_RNDN);\n"], [a, b, c])
instruction (ISqr a b) = tell (["mpfr_sqr(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ISin a b) = tell (["mpfr_sin(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ICos a b) = tell (["mpfr_cos(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (IExp a b) = tell (["mpfr_exp(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ILog a b) = tell (["mpfr_log(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ISinh a b) = tell (["mpfr_sinh(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ICosh a b) = tell (["mpfr_cosh(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ISqrt a b) = tell (["mpfr_sqrt(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])
instruction (ILog1p a b) = tell (["mpfr_log1p(", a, ",", b, ",MPFR_RNDN);\n"], [a, b])

instruction (IIfTE d c a b) = tell (["mpfr_set(", d, ",", c, "?", a, ":", b, ", MPFR_RNDN);\n"], [a, b, d])

compile (EAssign (EVar v) a) = do
  u <- compile a
  instruction (ISet v u)
  deallocate u
  return v

compile (EVar v) = do
  return v

compile (EAbs a) = do
  u <- compile a
  v <- allocate
  instruction (IAbs v u)
  deallocate u
  return v

compile (ENeg a) = do
  u <- compile a
  v <- allocate
  instruction (INeg v u)
  deallocate u
  return v

compile (ESgn a) = do
  u <- compile a
  v <- allocate
  instruction (ISgn v u)
  deallocate u
  return v

compile (ESub (EInt a) b) = do
  v <- compile b
  w <- allocate
  instruction (IISub w a v)
  deallocate v
  return w

compile (ESub a b) = do
  u <- compile a
  v <- compile b
  w <- allocate
  instruction (ISub w u v)
  deallocate u
  deallocate v
  return w

compile (EAdd (EInt a) b) = do
  v <- compile b
  w <- allocate
  instruction (IIAdd w a v)
  deallocate v
  return w

compile (EAdd a b) = do
  u <- compile a
  v <- compile b
  w <- allocate
  instruction (IAdd w u v)
  deallocate u
  deallocate v
  return w

compile (EMul (EInt a) b) = do
  v <- compile b
  w <- allocate
  instruction (IMulI w v a)
  deallocate v
  return w

compile (EMul a (EInt b)) = do
  u <- compile a
  w <- allocate
  instruction (IMulI w u b)
  deallocate u
  return w

compile (EMul (ENeg (EInt a)) b) = do
  v <- compile b
  w <- allocate
  instruction (IMulI w v (negate a))
  deallocate v
  return w

compile (EMul a (ENeg (EInt b))) = do
  u <- compile a
  w <- allocate
  instruction (IMulI w u (negate b))
  deallocate u
  return w

compile (EMul a b) = do
  u <- compile a
  v <- compile b
  w <- allocate
  instruction (IMul w u v)
  deallocate u
  deallocate v
  return w

compile (EDiv (EInt a) (EInt b)) = do
  w <- allocate
  instruction (IDivII w a b)
  return w

compile (EDiv a (EInt b)) = do
  u <- compile a
  w <- allocate
  instruction (IDivI w u b)
  deallocate u
  return w

compile (EDiv a b) = do
  u <- compile a
  v <- compile b
  w <- allocate
  instruction (IDiv w u v)
  deallocate u
  deallocate v
  return w

compile (ESqr a) = do
  u <- compile a
  v <- allocate
  instruction (ISqr v u)
  deallocate u
  return v

compile (ESin a) = do
  u <- compile a
  v <- allocate
  instruction (ISin v u)
  deallocate u
  return v

compile (ECos a) = do
  u <- compile a
  v <- allocate
  instruction (ICos v u)
  deallocate u
  return v

compile (ESinh a) = do
  u <- compile a
  v <- allocate
  instruction (ISinh v u)
  deallocate u
  return v

compile (ECosh a) = do
  u <- compile a
  v <- allocate
  instruction (ICosh v u)
  deallocate u
  return v

compile (EExp a) = do
  u <- compile a
  v <- allocate
  instruction (IExp v u)
  deallocate u
  return v

compile (ESqrt a) = do
  u <- compile a
  v <- allocate
  instruction (ISqrt v u)
  deallocate u
  return v

compile (ELog a) = do
  u <- compile a
  v <- allocate
  instruction (ILog v u)
  deallocate u
  return v

compile (ELog1p a) = do
  u <- compile a
  v <- allocate
  instruction (ILog1p v u)
  deallocate u
  return v

compile (EIf c (ETE a b)) = do
  u <- compile a
  v <- compile b
  w <- compile c
  x <- allocate
  instruction (IIfTE x w u v)
  deallocate u
  deallocate v
  deallocate w
  return x

compile (EGt a (EInt 0)) = do
  u <- compile a
  tell ([], [u])
  return $ "(mpfr_sgn(" ++ u ++ ") > 0)"

compile (ELt a (EInt 0)) = do
  u <- compile a
  tell ([], [u])
  return $ "(mpfr_sgn(" ++ u ++ ") < 0)"

compile (EDiffAbs c d) = compile (EIf (ELt c (EInt 0)) (ETE
    (EIf (EGt (EAdd c d) (EInt 0)) (ETE       (EAdd (EMul (EInt 2) c) d)  (ENeg d)))
    (EIf (ELt (EAdd c d) (EInt 0)) (ETE (ENeg (EAdd (EMul (EInt 2) c) d))       d))))

compile x = error $ "compile (" ++ show x ++ ")"

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
vars (EDiv a b) = vars a ++ vars b
vars (EAdd a b) = vars a ++ vars b
vars (ESub a b) = vars a ++ vars b
vars (EGt a b) = vars a ++ vars b
vars (ELt a b) = vars a ++ vars b
vars (ENeg a) = vars a
vars (ESgn a) = vars a
vars (EAbs a) = vars a
vars (ESqr a) = vars a
vars (EExp a) = vars a
vars (ESin a) = vars a
vars (ECos a) = vars a
vars (ESinh a) = vars a
vars (ECosh a) = vars a
vars (ESqrt a) = vars a
vars (ELog a) = vars a
vars (ELog1p a) = vars a
vars (EDiffAbs a b) = vars a ++ vars b
vars (ESpecial_93_2 a b c d) = vars a ++ vars b ++ vars c ++ vars d
vars (EAssign a b) = vars a ++ vars b
vars (EIf a b) = vars a ++ vars b
vars (ETE a b) = vars a ++ vars b

interpret _ (EInt n) = show n
interpret _ (EVar v) = v

interpret t@"" (EPow a b) = "(" ++ interpret t a ++ "^" ++ interpret t b ++ ")"
interpret t@"" (EMul a@(ENeg (EInt _)) b) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t@"" (EMul a b@(ENeg (EInt _))) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t@"" (EMul a@(EInt _) b) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t@"" (EMul a b@(EInt _)) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t@"" (EMul a b) = "(" ++ interpret t a ++ "*" ++ interpret t b ++ ")"
interpret t@"" (EDiv a@(EInt _) b@(EInt _)) = "T(" ++ interpret t a ++ ".0" ++ "/" ++ interpret t b ++ ".0" ++ ")" -- hack
interpret t@"" (EDiv a b) = "(" ++ interpret t a ++ "/" ++ interpret t b ++ ")"
interpret t@"" (EAdd a b@(ENeg (EInt _))) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t@"" (EAdd a b@(EInt _)) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t@"" (EAdd a@(ENeg (EInt _)) b) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t@"" (EAdd a@(EInt _) b) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t@"" (EAdd a b) = "(" ++ interpret t a ++ "+" ++ interpret t b ++ ")"
interpret t@"" (ESub a b@(ENeg (EInt _))) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t@"" (ESub a b@(EInt _)) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t@"" (ESub a@(ENeg (EInt _)) b) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t@"" (ESub a@(EInt _) b) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t@"" (ESub a b) = "(" ++ interpret t a ++ "-" ++ interpret t b ++ ")"
interpret t@"" (ELt a b) = "(" ++ interpret t a ++ "<" ++ interpret t b ++ ")"
interpret t@"" (EGt a b) = "(" ++ interpret t a ++ ">" ++ interpret t b ++ ")"
interpret t@"" (ENeg (EInt v)) = interpret t (EInt (negate v))
interpret t@"" (ENeg a) = "-(" ++ interpret t a ++ ")"
interpret t@"" (ESgn a) = "sgn(" ++ interpret t a ++ ")"
interpret t@"" (EAbs a) = "abs(" ++ interpret t a ++ ")"
interpret t@"" (ESqr a) = "sqr(" ++ interpret t a ++ ")"
interpret t@"" (EExp a) = "exp(" ++ interpret t a ++ ")"
interpret t@"" (ELog a) = "log(" ++ interpret t a ++ ")"
interpret t@"" (ELog1p a) = "log1p(" ++ interpret t a ++ ")"
interpret t@"" (ESin a) = "sin(" ++ interpret t a ++ ")"
interpret t@"" (ECos a) = "cos(" ++ interpret t a ++ ")"
interpret t@"" (ESinh a) = "sinh(" ++ interpret t a ++ ")"
interpret t@"" (ECosh a) = "cosh(" ++ interpret t a ++ ")"
interpret t@"" (ESqrt a) = "sqrt(" ++ interpret t a ++ ")"
interpret t@"" (EDiffAbs a b) = "diffabs(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t@"" (ESpecial_93_2 a b c d) = "special_93_2(" ++ interpret t a ++ "," ++ interpret t b ++ "," ++ interpret t c ++ "," ++ interpret t d ++ ")"
interpret t@"" (EAssign (EVar v@('t':'_':_)) a) = "const auto " ++ v ++ "=" ++ interpret t a ++ ";"
interpret t@"" (EAssign (EVar v) a) = v ++ "=" ++ interpret t a ++ ";"
interpret t@"" (EIf a (ETE b c)) = "(" ++ interpret t a ++ "?" ++ interpret t b ++ ":" ++ interpret t c ++ ")"

interpret t (EPow a b) = t ++ "powi(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a@(ENeg (EInt _)) b) = t ++ "imul(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a b@(ENeg (EInt _))) = t ++ "muli(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a@(EInt _) b) = t ++ "imul(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a b@(EInt _)) = t ++ "muli(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EMul a b) = t ++ "mul(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EDiv a b@(EInt _)) = t ++ "divi(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EDiv a b) = t ++ "div(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAdd a b@(ENeg (EInt _))) = t ++ "addi(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAdd a b@(EInt _)) = t ++ "addi(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAdd a@(ENeg (EInt _)) b) = t ++ "iadd(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAdd a@(EInt _) b) = t ++ "iadd(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EAdd a b) = t ++ "add(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ESub a b@(ENeg (EInt _))) = t ++ "subi(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ESub a b@(EInt _)) = t ++ "subi(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ESub a@(ENeg (EInt _)) b) = t ++ "isub(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ESub a@(EInt _) b) = t ++ "isub(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ESub a b) = t ++ "sub(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ELt a b) = t ++ "lt(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (EGt a b) = t ++ "gt(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ENeg (EInt v)) = interpret t (EInt (negate v))
interpret t (ENeg a) = t ++ "neg(" ++ interpret t a ++ ")"
interpret t (ESgn a) = t ++ "sgn(" ++ interpret t a ++ ")"
interpret t (EAbs a) = t ++ "abs(" ++ interpret t a ++ ")"
interpret t (ESqr a) = t ++ "sqr(" ++ interpret t a ++ ")"
interpret t (EExp a) = t ++ "exp(" ++ interpret t a ++ ")"
interpret t (ELog a) = t ++ "log(" ++ interpret t a ++ ")"
interpret t (ELog1p a) = t ++ "log1p(" ++ interpret t a ++ ")"
interpret t (ESin a) = t ++ "sin(" ++ interpret t a ++ ")"
interpret t (ECos a) = t ++ "cos(" ++ interpret t a ++ ")"
interpret t (ESinh a) = t ++ "sinh(" ++ interpret t a ++ ")"
interpret t (ECosh a) = t ++ "cosh(" ++ interpret t a ++ ")"
interpret t (ESqrt a) = t ++ "cosh(" ++ interpret t a ++ ")"
interpret t (EDiffAbs a b) = t ++ "diffabs(" ++ interpret t a ++ "," ++ interpret t b ++ ")"
interpret t (ESpecial_93_2 a b c d) = t ++ "special_93_2(" ++ interpret t a ++ "," ++ interpret t b ++ "," ++ interpret t c ++ "," ++ interpret t d ++ ")"
interpret t (EAssign (EVar v@('t':'_':_)) a) = (case t of
  ""     -> "const auto "
  "d_"   -> "const double "
  "dc_"  -> "const dcomplex "
  "fe_"  -> "const floatexp "
  "fec_" -> "const fecomplex "
  "sf_"  -> "const softfloat "
  "sfc_" -> "const sfcomplex ") ++ v ++ "=" ++ interpret t a ++ ";"
interpret t (EAssign (EVar v) a) = v ++ "=" ++ interpret t a ++ ";"
interpret t (EIf a (ETE b c)) = "(" ++ interpret t a ++ "?" ++ interpret t b ++ ":" ++ interpret t c ++ ")"

interpret t e = error $ "interpret " ++ show t ++ " (" ++ show e ++ ")"

prepare "d" vs = unlines . concat $
  [ [ "const auto Xr2 = Xr * Xr;" | "Xr2" `elem` vs ]
  , [ "const auto Xi2 = Xi * Xi;" | "Xi2" `elem` vs ]
  , [ "const auto xr2 = xr * xr;" | "xr2" `elem` vs ]
  , [ "const auto xi2 = xi * xi;" | "xi2" `elem` vs ]
  ]
prepare "dc" vs = unlines . concat $
  [ [ "const auto X2 = X * X;" | "X2" `elem` vs ]
  , [ "const auto x2 = x * x;" | "x2" `elem` vs ]
  , [ "const auto Xx2 = Xx * Xx;" | "Xx2" `elem` vs ]
  ]
prepare "cld" vs = unlines . concat $
  [ [ "const double Xr2 = d_mul(Xr, Xr);" | "Xr2" `elem` vs ]
  , [ "const double Xi2 = d_mul(Xi, Xi);" | "Xi2" `elem` vs ]
  , [ "const double xr2 = d_mul(xr, xr);" | "xr2" `elem` vs ]
  , [ "const double xi2 = d_mul(xi, xi);" | "xi2" `elem` vs ]
  ]
prepare "cldc" vs = unlines . concat $
  [ [ "const dcomplex X2 = dc_sqr(X);" | "X2" `elem` vs ]
  , [ "const dcomplex x2 = dc_sqr(x);" | "x2" `elem` vs ]
  , [ "const dcomplex Xx2 = dc_sqr(Xx);" | "Xx2" `elem` vs ]
  ]
prepare "clfe" vs = unlines . concat $
  [ [ "const floatexp Xr2 = fe_mul(Xr, Xr);" | "Xr2" `elem` vs ]
  , [ "const floatexp Xi2 = fe_mul(Xi, Xi);" | "Xi2" `elem` vs ]
  , [ "const floatexp xr2 = fe_mul(xr, xr);" | "xr2" `elem` vs ]
  , [ "const floatexp xi2 = fe_mul(xi, xi);" | "xi2" `elem` vs ]
  ]
prepare "clfec" vs = unlines . concat $
  [ [ "const fecomplex X2 = fec_sqr(X);" | "X2" `elem` vs ]
  , [ "const fecomplex x2 = fec_sqr(x);" | "x2" `elem` vs ]
  , [ "const fecomplex Xx2 = fec_sqr(Xx);" | "Xx2" `elem` vs ]
  ]
prepare "clsf" vs = unlines . concat $
  [ [ "const softfloat Xr2 = sf_sqr(Xr);" | "Xr2" `elem` vs ]
  , [ "const softfloat Xi2 = sf_sqr(Xi);" | "Xi2" `elem` vs ]
  , [ "const softfloat xr2 = sf_sqr(xr);" | "xr2" `elem` vs ]
  , [ "const softfloat xi2 = sf_sqr(xi);" | "xi2" `elem` vs ]
  ]
prepare "clsfc" vs = unlines . concat $
  [ [ "const sfcomplex X2 = sfc_sqr(X);" | "X2" `elem` vs ]
  , [ "const sfcomplex x2 = sfc_sqr(x);" | "x2" `elem` vs ]
  , [ "const sfcomplex Xx2 = sfc_sqr(Xx);" | "Xx2" `elem` vs ]
  ]

def :: GenLanguageDef String () Identity
def = emptyDef{ identStart = letter
              , identLetter = alphaNum <|> char '_'
              , opStart = oneOf ops
              , opLetter = oneOf ops
              , reservedOpNames = map (:[]) ops
              , reservedNames = ["exp", "log", "log1p", "sin", "cos", "sinh", "cosh", "sqrt", "sqr", "sgn", "abs", "diffabs", "special_93_2"]
              }
  where ops = "=?:<>+-*^"

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = op
           , reserved = m_reserved
           , integer = m_integer } = makeTokenParser def

exprparser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (op "-" >> return ENeg)]
        , [Infix (op "^" >> return EPow) AssocLeft]
        , [Infix (op "*" >> return EMul) AssocLeft
          ,Infix (op "/" >> return EDiv) AssocLeft]
        , [Infix (op "+" >> return EAdd) AssocLeft
          ,Infix (op "-" >> return ESub) AssocLeft]
        , [Infix (op ">" >> return EGt) AssocLeft
          ,Infix (op "<" >> return ELt) AssocLeft]
        , [Infix (op ":" >> return ETE) AssocLeft]
        , [Infix (op "?" >> return EIf) AssocLeft]
        , [Infix (op "=" >> return EAssign) AssocLeft]
        ]
term = m_parens exprparser
       <|> (EVar <$> m_identifier)
       <|> (EInt . fromIntegral <$> m_integer)
       <|> (char '-' >> EInt . negate . fromIntegral <$> m_integer)
       <|> (m_reserved "sinh" >> ESinh <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "cosh" >> ECosh <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "sin" >> ESin <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "cos" >> ECos <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "exp" >> EExp <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "log1p" >> ELog1p <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "log" >> ELog <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "sqr" >> ESqr <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "sqrt" >> ESqrt <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "sgn" >> ESgn <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "abs" >> EAbs <$ string "(" <*> exprparser <* string ")")
       <|> (m_reserved "diffabs" >> EDiffAbs <$ string "(" <*> exprparser <* string "," <*> exprparser <* string ")")
       <|> (m_reserved "special_93_2" >> ESpecial_93_2 <$ string "(" <*> exprparser <* string "," <*> exprparser <* string "," <*> exprparser <* string "," <*> exprparser <* string ")")

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
  Right es -> "{" ++ prepare t (nub $ concatMap vars es) ++ unlines (map (interpret t2) es) ++ "}"
  Left e -> error (show e ++ " : " ++ show s)
  where
    t2 | t == "d" = ""
       | t == "dc" = ""
       | otherwise = case t of 'c':'l':ts -> ts ++ "_"

main = interact parseCL
