import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (dropExtension)
import Text.Printf (printf)
import Text.Parsec
import Text.Parsec.String (Parser)

-- main :: IO ()
-- main = do
--   srcs <- getArgs
--   let dsts = map (replaceExt "dat") srcs
--   bins <- sequence $ map emitAsm srcs
--   zipWithM_ dumpAsm dsts bins

type HexCode  = [String]

class Eq a => AsmOp a where
  opTable   :: [(String, a)]
  -- funct: R, opcode: I and J
  -- storaged as string of hex number
  hexTable  :: [(a, Int)]
  evalOp    :: a -> IO Int
  parseOp   :: Parser a

  evalOp op = case lookup op hexTable of
                Just hex -> return hex
                Nothing  -> die "This operation is not implemented"
  parseOp   = choice opParsers
    where opParsers = map (\(a, b) -> try $ string a >> return b) opTable

data AsmOpR = And | Or | Add | Sub
            deriving (Show, Eq)
instance AsmOp AsmOpR where
  opTable   = [ ("and", And)
              , ("or",  Or)
              , ("add", Add)
              , ("sub", Sub)
              ]
  hexTable  = [ (And, 0x24)
              , (Or,  0x25)
              , (Add, 0x20)
              , (Sub, 0x22)
              ]

data AsmOpI = Lw | Sw | Beq
            deriving (Show, Eq)
instance AsmOp AsmOpI where
  opTable   = [ ("lw",  Lw)
              , ("sw",  Sw)
              , ("beq", Beq)
              ]
  hexTable  = [ (Lw,  0x23)
              , (Sw,  0x2b)
              , (Beq, 0x4)
              ]

data AsmOpJ = J
            deriving (Show, Eq)
instance AsmOp AsmOpJ where
  opTable   = [ ("j", J)
              ]
  hexTable  = [ (J, 0x2)
              ]

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


parseOpR :: Parser AsmOpR
parseOpR = parseOp :: Parser AsmOpR

parseOpI :: Parser AsmOpI
parseOpI = parseOp :: Parser AsmOpI

parseOpJ :: Parser AsmOpJ
parseOpJ = parseOp :: Parser AsmOpJ

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
parseExpr  =  try parseLabel
          <|> try parseOprR
          <|> try parseOprI
          <|> try parseOprJ
  where
    parseLabel = do
      spaces
      name <- parseVal
      char ':'
      return $ Label name
    parseOprR = do
      spaces
      op <- parseOpR; spaces
      x1 <- parseVal; spaces >> char ',' >> spaces
      x2 <- parseVal; spaces >> char ',' >> spaces
      x3 <- parseVal; spaces
      return $ OprR op x1 x2 x3
    parseOprI = try parseComma <|> try parseBrace
      where
        parseComma = do
          spaces
          op <- parseOpI; spaces
          x1 <- parseVal; spaces >> char ',' >> spaces
          x2 <- parseVal; spaces >> char ',' >> spaces
          x3 <- parseVal; spaces
          return $ OprI op x1 x2 x3
        parseBrace = do
          spaces
          op <- parseOpI; spaces
          x1 <- parseVal; spaces >> char ',' >> spaces
          x2 <- parseVal; spaces
          char '(' >> spaces
          x3 <- parseVal; spaces
          char ')' >> spaces
          return $ OprI op x1 x2 x3
    parseOprJ = do
      spaces
      op <- parseOpJ; spaces
      x1 <- parseVal; spaces
      return $ OprJ op x1

dumpAsm :: FilePath -> HexCode -> IO ()
dumpAsm dst bin = writeFile dst $ unlines bin

-- emitAsm :: FilePath -> IO HexCode

-- evalOpR  :: AsmOpR  -> IO String
-- evalOpI  :: AsmOpI  -> IO String
-- evalOpJ  :: AsmOpJ  -> IO String
-- evalVal  :: AsmVal  -> IO String
-- evalExpr :: AsmExpr -> IO String

replaceExt :: String -> FilePath -> FilePath
replaceExt new path = body ++ new
  where body = dropExtension path

