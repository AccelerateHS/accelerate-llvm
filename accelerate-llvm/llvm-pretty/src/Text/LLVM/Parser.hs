module Text.LLVM.Parser where

import Text.LLVM.AST

import Data.Char (chr)
import Data.Word (Word32, Word64)
import Text.Parsec
import Text.Parsec.String


-- Identifiers and Symbols -----------------------------------------------------

pNameChar :: Parser Char
pNameChar = letter <|> digit <|> oneOf "-$._"

pHexEscape :: Parser Char
pHexEscape =
  do _ <- char '\\'
     a <- hexDigit
     b <- hexDigit
     return (chr (read ("0x" ++ [a, b])))

pStringChar :: Parser Char
pStringChar = noneOf "\"\\" <|> pHexEscape

pName :: Parser String
pName = many1 pNameChar <|> quotes (many1 pStringChar)

pIdent :: Parser Ident
pIdent = Ident <$> (char '%' >> pName)

pSymbol :: Parser Symbol
pSymbol = Symbol <$> (char '@' >> pName)


-- Types -----------------------------------------------------------------------

pWord32 :: Parser Word32
pWord32 = read <$> many1 digit

pWord64 :: Parser Word64
pWord64 = read <$> many1 digit

pPrimType :: Parser PrimType
pPrimType = choice
  [ Integer <$> try (char 'i' >> pWord32)
  , FloatType <$> try pFloatType
  , try (string "label")    >> return Label
  , try (string "void")     >> return Void
  , try (string "x86mmx")   >> return X86mmx
  , try (string "metadata") >> return Metadata
  ]

pFloatType :: Parser FloatType
pFloatType = choice
  [ try (string "half")      >> return Half
  , try (string "float")     >> return Float
  , try (string "double")    >> return Double
  , try (string "fp128")     >> return Fp128
  , try (string "x86_fp80")  >> return X86_fp80
  , try (string "ppc_fp128") >> return PPC_fp128
  ]

pType :: Parser Type
pType = pType0 >>= pFunPtr
  where
    pType0 :: Parser Type
    pType0 =
      choice
      [ Alias <$> pIdent
      , brackets (pNumType Array)
      , braces (Struct <$> pTypeList)
      , angles (braces (PackedStruct <$> pTypeList) <|> spaced (pNumType Vector))
      , string "opaque" >> return Opaque
      , PrimType <$> pPrimType
      , pOpaquePtr
      ]

    pTypeList :: Parser [Type]
    pTypeList = sepBy (spaced pType) (char ',')

    pNumType :: (Word64 -> Type -> Type) -> Parser Type
    pNumType f =
      do n <- pWord64
         spaces >> char 'x' >> spaces
         t <- pType
         return (f n t)

    pArgList :: Type -> Parser Type
    pArgList t0 = spaces >> (p1 [] <|> return (FunTy t0 [] False))
      where
        p1 ts =
          (string "..." >> spaces >> return (FunTy t0 (reverse ts) True))
          <|> (pType >>= \t -> (spaces >> p2 (t : ts)))
        p2 ts =
          (char ',' >> spaces >> p1 ts)
          <|> return (FunTy t0 (reverse ts) False)

    pOpaquePtr :: Parser Type
    pOpaquePtr = do
      _ <- string "ptr"
      PtrOpaque <$> option defaultAddrSpace (spaces >> pAddrSpace)

    pFunPtr :: Type -> Parser Type
    pFunPtr t0 = pFun <|> pPtr <|> return t0
      where
        pFun = parens (pArgList t0) >>= pFunPtr
        pPtr = do
          addr <- option defaultAddrSpace (spaces >> pAddrSpace)
          _ <- char '*'
          pFunPtr (PtrTo t0 addr)

    pAddrSpace :: Parser AddrSpace
    pAddrSpace = do
      _ <- string "addrspace("
      n <- pWord32
      _ <- string ")"
      return (AddrSpace n)

parseType :: String -> Either ParseError Type
parseType = parse (pType <* eof) "<internal>"


-- Utilities -------------------------------------------------------------------

angles :: Parser a -> Parser a
angles body = char '<' *> body <* char '>'

braces :: Parser a -> Parser a
braces body = char '{' *> body <* char '}'

brackets :: Parser a -> Parser a
brackets body = char '[' *> body <* char ']'

parens :: Parser a -> Parser a
parens body = char '(' *> body <* char ')'

quotes :: Parser a -> Parser a
quotes body = char '"' *> body <* char '"'

spaced :: Parser a -> Parser a
spaced body = spaces *> body <* spaces
