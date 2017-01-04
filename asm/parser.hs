import Control.Monad      (zipWithM_)
import System.Environment (getArgs)
import System.Exit        (die)
import System.FilePath    (dropExtension)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Printf        (printf)

main :: IO ()
main = do
  srcs <- getArgs
  dsts <- return $ map (replaceExt ".dat") srcs
  bins <- mapM emitAsm srcs
  zipWithM_ dumpAsm dsts bins

type BinCode  = String

class Eq a => AsmOp a where
  opTable   :: [(String, a)]
  -- funct: R, opcode: I and J
  -- storaged as string of hex number
  hexTable  :: [(a, Int)]

  evalOp    :: Int -> a -> BinCode
  evalOp len op = case lookup op hexTable of
                    Just hex -> printf ("%0"++(show len)++"b") hex
                    Nothing  -> ""
  parseOp   :: Parser a
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
      rd <- parseVal; spaces >> char ',' >> spaces
      rs <- parseVal; spaces >> char ',' >> spaces
      rt <- parseVal; spaces
      return $ OprR op rd rs rt
    parseOprI = try parseComma <|> try parseBrace
      where
        parseComma = do
          spaces
          op <- parseOpI; spaces
          rs <- parseVal; spaces >> char ',' >> spaces
          rt <- parseVal; spaces >> char ',' >> spaces
          cv <- parseVal; spaces
          return $ OprI op rs rt cv
        parseBrace = do
          spaces
          op <- parseOpI; spaces
          rs <- parseVal; spaces >> char ',' >> spaces
          cv <- parseVal; spaces
          char '(' >> spaces
          rt <- parseVal; spaces
          char ')' >> spaces
          return $ OprI op rs rt cv
    parseOprJ = do
      spaces
      op <- parseOpJ; spaces
      cv <- parseVal; spaces
      return $ OprJ op cv

dumpAsm :: FilePath -> [BinCode] -> IO ()
dumpAsm dst bin = writeFile dst formedBin
  where
    isValid line = (length line) == 32
    packedBin = filter isValid bin
    formedBin = unlines packedBin
 
emitAsm :: FilePath -> IO [BinCode]
emitAsm src = do
  code      <- readFile src
  codeLines <- return $ lines code
  lineAsts  <- mapM parseIO codeLines
  binLines  <- return $ map evalExpr lineAsts
  return binLines
    where
      parseIO :: String -> IO AsmExpr
      parseIO code = case parse parseExpr "asm" code of
        Right ast -> return ast
        Left err  -> die "Failed parsing"

evalOpR :: Int -> AsmOpR -> BinCode
evalOpR = evalOp :: Int -> AsmOpR -> BinCode

evalOpI :: Int -> AsmOpI -> BinCode
evalOpI = evalOp :: Int -> AsmOpI -> BinCode

evalOpJ :: Int -> AsmOpJ -> BinCode
evalOpJ = evalOp :: Int -> AsmOpJ -> BinCode

-- TODO: Implement dictionary-based evaluation of Name and Var
evalVal :: Int -> AsmVal  -> BinCode
evalVal len (Name n) = printf ("%0"++(show len)++"b") (0 :: Int)
evalVal len (Var v)  = printf ("%0"++(show len)++"b") (0 :: Int)
evalVal len (Imm i)  = printf ("%0"++(show len)++"b") i

evalExpr :: AsmExpr -> BinCode
evalExpr (Label l) = ""

evalExpr (OprR op rd rs rt) = "000000"
                           ++ evalVal 5 rs
                           ++ evalVal 5 rt
                           ++ evalVal 5 rd
                           ++ "00000"
                           ++ evalOpR 6 op

evalExpr (OprI op rs rt cv) = evalOpI 6 op
                           ++ evalVal 5 rs
                           ++ evalVal 5 rt
                           ++ evalVal 16 cv

evalExpr (OprJ op cv) = evalOpJ 6 op
                     ++ evalVal 26 cv

replaceExt :: String -> FilePath -> FilePath
replaceExt new path = body ++ new
  where body = dropExtension path

