import Control.Monad      (liftM, zipWithM_)
import Data.Bits
import System.Environment (getArgs)
import System.Exit        (die)
import System.FilePath    (dropExtension)
import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser)
import Text.Printf        (printf)

main :: IO ()
main = do
  srcs <- getArgs
  dsts <- return $ map (replaceExt ".dat") srcs
  bins <- mapM emitAsm srcs
  zipWithM_ dumpAsm dsts bins

------------------------------------------------------------
-- Type Definitions
------------------------------------------------------------

type BinCode  = String

class Eq a => AsmOp a where
  opTable   :: [(String, a)]
  -- funct: R, opcode: I and J
  -- storaged as Int of hex number
  hexTable  :: [(a, Int)]

  evalOp    :: Int -> a -> BinCode
  evalOp len op = case lookup op hexTable of
                    Just hex -> printf ("%0"++(show len)++"b") hex
                    Nothing  -> ""
  parseOp   :: Parser a
  parseOp   = choice opParsers
    where opParsers = map (\(a, b) -> try $ string a >> return b) opTable

data AsmOpR = Add | Addu | And | Jr | Nor | Or
            | Slt | Sltu | Sll | Srl | Sub | Subu
            deriving (Show, Eq)
instance AsmOp AsmOpR where
  opTable   = [ ("add", Add), ("addu", Addu), ("and", And), ("jr", Jr)
              , ("nor", Nor), ("or", Or),     ("slt", Slt), ("sltu", Sltu)
              , ("sll", Sll), ("srl", Srl),   ("sub", Sub), ("subu", Subu)
              ]
  hexTable  = [ (Add, 0x20), (Addu, 0x21), (And, 0x24), (Jr, 0x8)
              , (Nor, 0x27), (Or, 0x25),   (Slt, 0x2a), (Sltu, 0x2b)
              , (Sll, 0x00), (Srl, 0x2),   (Sub, 0x22), (Subu, 0x23)
              ]

data AsmOpI = Addi | Addi | Andi | Beq | Bne | Lbu
            | Lhu | Ll | Lui | Lw | Ori | Slti
            | Sltiu | Sb | Sc | Sh | Sw
            deriving (Show, Eq)
instance AsmOp AsmOpI where
  opTable   = [ ("addi", Addi),   ("addiu", Addiu), ("andi", Andi), ("beq", Beq)
              , ("bne", Bne),     ("lbu", Lbu),     ("lhu", Lhu),   ("ll", Ll)
              , ("lui", Lui),     ("lw", Lw),       ("ori", Ori),   ("slti", Slti)
              , ("sltiu", Sltiu), ("sb", Sb),       ("sc", Sc),     ("sh", Sh)
              , ("sw", Sw)
              ]
  hexTable  = [ (Addi, 0x8),  (Addiu, 0x9), (Andi, 0xc), (Beq, 0x4)
              , (Bne, 0x5),   (Lbu, 0x24),  (Lhu, 0x25), (Ll, 0x30)
              , (Lui, 0xf),   (Lw, 0x23),   (Ori, 0xd),  (Slti, 0xa)
              , (Sltiu, 0xb), (Sb, 0x28),   (Sc, 0x38),  (Sh, 0x29)
              , (Sw, 0x2b)
              ]

data AsmOpJ = J | Jal
            deriving (Show, Eq)
instance AsmOp AsmOpJ where
  opTable   = [ ("j",   J) , ("jal", Jal)
              ]
  hexTable  = [ (J, 0x2), (Jal, 0x3)
              ]

data AsmVal   = Imm   Int
              | Var   Int
              | Name  String
              deriving (Show, Eq)

varTable :: [(String, Int)]
varTable = [ ("$zero", 0), ("$at", 1),  ("$v0", 2),  ("$v1", 3)
           , ("$a0", 4),   ("$a1", 5),  ("$a2", 6),  ("$a3", 7)
           , ("$t0", 8),   ("$t1", 9),  ("$t2", 10), ("$t3", 11)
           , ("$t4", 12),  ("$t5", 13), ("$t6", 14), ("$t7", 15)
           , ("$s0", 16),  ("$s1", 17), ("$s2", 18), ("$s3", 19)
           , ("$s4", 20),  ("$s5", 21), ("$s6", 22), ("$s7", 23)
           , ("$t8", 24),  ("$t9", 25), ("$k0", 26), ("$k1", 27)
           , ("$gp", 28),  ("$sp", 29), ("$fp", 30), ("$ra", 31)
           ]

type NameEnv = [(AsmVal, Int)]

data AsmExpr  = Ignore
              | Label AsmVal
              | OprR  AsmOpR AsmVal AsmVal AsmVal
              | OprI  AsmOpI AsmVal AsmVal AsmVal
              | OprJ  AsmOpJ AsmVal
              deriving (Show, Eq)

type AsmProg = [AsmExpr]

------------------------------------------------------------
-- Parser Definitions
------------------------------------------------------------

runParse :: String -> IO AsmProg
runParse code = case parse parseProg "asm" code of
  Right ast -> return ast
  Left err  -> die $ show err

parseOpR :: Parser AsmOpR
parseOpR = parseOp :: Parser AsmOpR

parseOpI :: Parser AsmOpI
parseOpI = parseOp :: Parser AsmOpI

parseOpJ :: Parser AsmOpJ
parseOpJ = parseOp :: Parser AsmOpJ

spaces :: Parser ()
spaces = skipMany (space <|> tab)

parseVal :: Parser AsmVal
parseVal  =  parseName
         <|> parseVar
         <|> parseImm
  where
    parseImm  = do
      num <- many1 digit
      return $ Imm $ read num
    parseVar  = try parseStr <|> try parseInt
      where
        parseStr = do
          first  <- char '$'
          second <- letter
          rest   <- many (letter <|> digit)
          var    <- return $ first : second : rest
          return $ Var $ case lookup var varTable of
                           Just num -> num
        parseInt = do
          first <- char '$'
          rest <- many digit
          return $ Var $ read rest
    parseName = do
      first <- letter <|> char '_'
      rest  <- many (letter <|> digit <|> char '_')
      name  <- return $ first : rest
      return $ Name name

parseExpr :: Parser AsmExpr
parseExpr  =  try parseIgnore
          <|> try parseLabel
          <|> try parseOprR
          <|> try parseOprI
          <|> try parseOprJ
  where
    parseIgnore = do
      spaces
      char '#'
      many (noneOf "\n")
      return Ignore
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
          rt <- parseVal; spaces >> char ',' >> spaces
          cv <- parseVal; spaces
          char '(' >> spaces
          rs <- parseVal; spaces
          char ')' >> spaces
          return $ OprI op rs rt cv
    parseOprJ = do
      spaces
      op <- parseOpJ; spaces
      cv <- parseVal; spaces
      return $ OprJ op cv

parseProg :: Parser AsmProg
parseProg = many parseExpr >>= return

------------------------------------------------------------
-- Evaluation Functions
------------------------------------------------------------

evalOpR :: Int -> AsmOpR -> BinCode
evalOpR = evalOp :: Int -> AsmOpR -> BinCode

evalOpI :: Int -> AsmOpI -> BinCode
evalOpI = evalOp :: Int -> AsmOpI -> BinCode

evalOpJ :: Int -> AsmOpJ -> BinCode
evalOpJ = evalOp :: Int -> AsmOpJ -> BinCode

evalVal :: Int -> NameEnv -> AsmVal -> BinCode
evalVal len env (Imm i)  = printf ("%0"++(show len)++"b") i
evalVal len env (Var v)  = printf ("%0"++(show len)++"b") v
evalVal len env (Name n) = printf "%d" $ findEnv env (Name n)

evalExpr :: NameEnv -> Int -> AsmExpr -> BinCode
evalExpr env line (Label l) = ""
evalExpr env line (Ignore) = ""

evalExpr env line (OprR op rd rs rt)
  | op `elem` [Jr]
      =  "000000"
      ++ evalVal 5 env rs
      ++ "00000"
      ++ "00000"
      ++ "00000"
      ++ evalOpR 6 op
  -- Differs from the definition of patterson & hennessy (diff rs, rt)
  | op `elem` [Sll, Srl]
      =  "000000"
      ++ "00000"
      ++ evalVal 5 env rs
      ++ evalVal 5 env rd
      ++ evalVal 5 env rt
      ++ evalOpR 6 op
  | otherwise
      =  "000000"
      ++ evalVal 5 env rs
      ++ evalVal 5 env rt
      ++ evalVal 5 env rd
      ++ "00000"
      ++ evalOpR 6 op

evalExpr env line (OprI Beq rs rt cv)
  | op `elem` [Beq, Bne]
      =  evalOpI 6  op
      ++ evalVal 5  env rs
      ++ evalVal 5  env rt
      ++ evalVal_16_env_cv'
  | otherwise
      =  evalOpI 6  op
      ++ evalVal 5  env rs
      ++ evalVal 5  env rt
      ++ evalVal 16 env cv
  where
    cv'  = (read $ evalVal 16 env cv) - (line+1)
    comp len x | x < 0     = ((-x) `xor` (2^len - 1)) + 1
               | otherwise = x
    evalVal_16_env_cv' = printf "%016b" $ comp 16 cv'

evalExpr env line (OprJ op cv)
  = evalOpJ 6  op
 ++ evalVal_26_env_cv'
  where
    cv' = ((line+1)                   .&. 0xf0000000)
        + ((read $ evalVal 26 env cv) .&. 0x0fffffff)
    evalVal_26_env_cv' = printf "%026b" cv'

------------------------------------------------------------
-- Output Functions
------------------------------------------------------------

nullEnv :: NameEnv
nullEnv = []

makeEnv :: (AsmExpr, Int) -> NameEnv -> NameEnv
makeEnv (Label name, line) env = (name, line) : env
makeEnv (_, _) env = env

findEnv :: NameEnv -> AsmVal -> Int
findEnv env name = case lookup name env of
                     Just line -> line
                     Nothing   -> 0

dumpAsm :: FilePath -> [BinCode] -> IO ()
dumpAsm dst bin = writeFile dst formedBin
  where
    isValid line = (length line) == 32
    packedBin = filter isValid bin
    formedBin = unlines packedBin

countInst :: AsmExpr -> [Int] -> [Int]
countInst (Label l) (x:xs) = x : (x:xs)
countInst Ignore    (x:xs) = x : (x:xs)
countInst _         (x:xs) = (x+1) : (x:xs)

emitAsm :: FilePath -> IO [BinCode]
emitAsm src = do
  code <- readFile src
  asts <- runParse code
  let nums = reverse $ foldr countInst (0:[]) $ reverse asts
      env  = foldr makeEnv nullEnv $ zip asts nums
      bins = zipWith (evalExpr env) nums asts
  print env
  mapM_ print $ zip3 nums bins asts
  return bins

replaceExt :: String -> FilePath -> FilePath
replaceExt new path = body ++ new
  where body = dropExtension path

