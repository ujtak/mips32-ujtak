import System.Environment
import System.FilePath
import Text.Printf
import Text.Parsec
import Text.Parsec.String

-- main :: IO ()
-- main = do
--   srcs <- getArgs
--   let dsts = map (replaceExt "dat") srcs
--   bins <- sequence $ map emitAsm srcs
--   zipWithM_ dumpAsm dsts bins

type BinCode  = [String]

class AsmOp a where
  opTable :: [(String, a)]

data AsmOp    = And | Or | Add | Sub | Lw | Sw | Beq | J
              deriving Show

data AsmVal   = Name  String
              | Var   String
              | Imm   Int
              | Addr  Int -- for j instruction
              deriving Show

data AsmExpr  = Label AsmVal
              | OprR  AsmOpR AsmVal AsmVal AsmVal
              | OprI  AsmOpI AsmVal AsmVal AsmVal
              | OprJ  AsmOpJ AsmVal
              deriving Show

opParsers :: AsmOp a => [(String, a)] -> Parser a
opParsers = map (\(a, b) -> string a >> return b) opTable

parseOp :: Parser AsmOp
parseOp = foldl1 (<|>) opParsers
  where opTable = [ ("and", And)
                  , ("or", Or)
                  , ("add", Add)
                  , ("sub", Sub)
                  , ("lw", Lw)
                  , ("sw", Sw)
                  , ("beq", Beq)
                  , ("j", J)
                  ]

parseVal :: Parser AsmVal
parseVal  =  parseName
         <|> parseVar
         <|> parseImm
  where
    parseName = do
      first <- letter <|> char '_'
      rest  <- many (letter <|> digit <|> char '_')
      return $ Name $ first : rest
    parseVar  = do
      first <- char '$'
      rest  <- many (letter <|> digit <|> char '_')
      return $ Var $ first : rest
    parseImm  = do
      num <- many1 digit
      return $ Imm $ read num

parseExpr :: Parser AsmExpr
parseExpr  =  parseLabel
          <|> parseOprR
          <|> parseOprI
          <|> parseOprJ
  where
    parseLabel  = do
      name <- parseVal
      char ':'
      return $ Label name
    parseOprR   = do
      spaces
      op <- parseOpR; spaces
      x1 <- parseVal; char ',' >> spaces
      x2 <- parseVal; char ',' >> spaces
      x3 <- parseVal; string "\n"
      return $ OprR op x1 x2 x3
    parseOprI   = do
      spaces
      op <- parseOpI; spaces
      x1 <- parseVal; char ',' >> spaces
      x2 <- parseVal; char ',' >> spaces
      x3 <- parseVal; string "\n"
      return $ OprI op x1 x2 x3
    parseOprJ   = do
      spaces
      op <- parseOpJ; spaces
      x1 <- parseVal; string "\n"
      return $ OprJ op x1

dumpAsm :: FilePath -> BinCode -> IO ()
dumpAsm dst bin = writeFile dst $ unlines bin

-- emitAsm :: FilePath -> IO BinCode

-- evalExpr :: AsmExpr -> String

-- evalOp :: AsmOp -> String

-- evalVal :: AsmVal -> String

replaceExt :: String -> FilePath -> FilePath
replaceExt new path = body ++ new
  where body = dropExtension path

