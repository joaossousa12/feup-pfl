module Main where

import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

-- for parser
import Data.Char (isSpace)
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token



-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackData = Value Integer | Boolean Bool | Expression String
    deriving (Show, Eq)

type Stack = [StackData]

type State = [(String, StackData)]

createEmptyStack :: Stack
createEmptyStack = [] 

-- converts StackData to its string representation.
stackDataToStr :: StackData -> String
stackDataToStr (Value i) = show i -- pass integer to string 
stackDataToStr (Boolean b) = if b then "True" else "False"
stackDataToStr (Expression s) = s

-- stack2Str works by:
-- 1. Reversing the stack (necessary to maintain the original order in the resulting string)
-- 2. Mapping each element of the stack to its string representation using stackDataToStr
-- 3. Concatenating the resulting list of strings with commas
stack2Str :: Stack -> String
stack2Str = intercalate "," . reverse . map stackDataToStr . reverse

createEmptyState :: Main.State
createEmptyState = []

-- state2Str works by:
-- 1. Sorting the state by variable names (sorted order for better readability)
-- 2. Mapping each key-value pair to its string representation using pairToStr
-- 3. Concatenating the resulting list of strings with commas
state2Str :: Main.State -> String
state2Str state = intercalate "," $ map pairToStr $ sortBy (comparing fst) state
  where
    pairToStr :: (String, StackData) -> String
    pairToStr (var, val) = var ++ "=" ++ stackDataToStr val

run :: (Code, Stack, Main.State) -> (Code, Stack, Main.State)
run ([], stack, state) = ([], stack, state)
run ((head:tail), stack, state) = case head of
  Push n -> run (tail, Value n : stack, state)
  Add -> run (tail, performArithmeticOp (+) stack, state)
  Sub -> run (tail, performArithmeticOp (-) stack, state)
  Mult -> run (tail, performArithmeticOp (*) stack, state)
  Tru -> run (tail, Boolean True : stack, state)
  Fals -> run (tail, Boolean False : stack, state)
  Fetch x -> 
    let val = lookup x state in
      case val of
        Just v -> run (tail, v : stack, state)
        Nothing -> error "Runtime error: Variable not found!"
  Store x -> 
    case stack of
      (v:tailStore) -> run (tail, tailStore, Main.updateState x v state)
      [] -> error "Runtime error: Stack underflow on Store!"
  Neg -> 
    case stack of
      (Boolean b : rest) -> run (tail, Boolean (not b) : rest, state)
      _ -> error "Runtime error: Negation applied to non-boolean value!" 
  Equ -> 
    case stack of
      (x : y : rest) -> run (tail, Boolean (x == y) : rest, state)
      _ -> error "Runtime error: Insufficient elements on stack for Equ!"
  Le -> 
    case stack of
      (Value x : Value y : rest) -> run (tail, Boolean (x <= y) : rest, state)
      _ -> error "Runtime error: Invalid elements on stack for Le or insufficient elements!"
  Noop -> run (tail, stack, state)
  Branch c1 c2 -> 
    case stack of
      (Boolean True : rest) -> run (c1 ++ tail, rest, state)
      (Boolean False : rest) -> run (c2 ++ tail, rest, state)
      _ -> error "Runtime error: Non-boolean value on stack for Branch!"
  Loop c1 c2 -> 
    let loopExp = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] in
    run (loopExp ++ tail, stack, state)
  And -> 
    case stack of
      (Boolean x : Boolean y : rest) -> run (tail, Boolean (x && y) : rest, state)
      _ -> error "Runtime error: And operation requires two boolean values!"

-- Helper function to perform arithmetic operations on the stack
performArithmeticOp :: (Integer -> Integer -> Integer) -> Stack -> Stack
performArithmeticOp op (Value x : Value y : tail) = Value (op x y) : tail
performArithmeticOp _ _ = error "Runtime error: Invalid stack for arithmetic operation!"

-- Helper function to update the state with a new value for a variable
updateState :: String -> StackData -> Main.State -> Main.State
updateState var val [] = [(var, val)]
updateState var val ((v, _):tail) | v == var = (var, val) : tail
updateState var val (pair:tail) = pair : Main.updateState var val tail

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Part 2

data Aexp
  = ANum Integer
  | AVar String
  | AAdd Aexp Aexp
  | ASub Aexp Aexp
  | AMul Aexp Aexp
  deriving (Show)

data Bexp
  = BTrue
  | BFalse
  | BNot Bexp
  | BAnd Bexp Bexp
  | BLe Aexp Aexp
  | BEq Aexp Aexp
  deriving (Show)

-- Statements
data Stm
  = Assign String Aexp
  | Seq [Stm]
  | If Bexp Stm Stm
  | While Bexp Stm
  deriving (Show)

-- Program (list of statements)
type Program = [Stm]

compA :: Aexp -> Code
compA (ANum n) = [Push n]
compA (AVar x) = [Fetch x]
compA (AAdd e1 e2) = compA e1 ++ compA e2 ++ [Add]
compA (ASub e1 e2) = compA e1 ++ compA e2 ++ [Sub]
compA (AMul e1 e2) = compA e1 ++ compA e2 ++ [Mult]

compB :: Bexp -> Code
compB (BLe e1 e2) = compA e2 ++ compA e1 ++ [Le]
compB (BEq e1 e2) = compA e2 ++ compA e1 ++ [Equ]
compB (BAnd b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (BNot b) = compB b ++ [Neg]
compB BTrue = [Tru]
compB BFalse = [Fals]

compile :: Program -> Code
compile [] = []
compile (stmt : rest) = case stmt of
  Assign x aexp -> compA aexp ++ [Store x] ++ compile rest
  Seq stmts -> concatMap compile [stmts] ++ compile rest
  If bexp stm1 stm2 -> compB bexp ++ [Branch (compile [stm1]) (compile [stm2])] ++ compile rest
  While bexp stm -> [Loop (compB bexp) (compile [stm])] ++ compile rest

-- parser stuff

-- not gonna use this for now
-- lexer :: String -> [String]
-- lexer [] = []
-- lexer s@(c:cs)
--   | c == ' ' = lexer (dropWhile (== ' ') cs)
--   | c == ';' = [";"] ++ lexer cs
--   | otherwise = let (word, rest) = span (\x -> x /= ' ' && x /= ';') s
--                 in word : lexer rest

-- notes for later: 
--                  - subtractions might be inverted 😅
--                  - adding a negative number (i.e. 8 + (-4)) doesn't work
aexpParser :: Parser Aexp
aexpParser = buildExpressionParser operators term <?> "expression"
  where
    operators = [ [Infix (AMul <$ spaces <* char '*' <* spaces) AssocLeft]
                , [Infix (AAdd <$ spaces <* char '+' <* spaces) AssocLeft
                  , Infix (ASub <$ spaces <* char '-' <* spaces) AssocLeft] ]
    term = try (ANum <$> (char '-' *> integer <|> integer))
       <|> try (AVar <$> identifier)
       <|> parens aexpParser
    parens = between (char '(') (char ')')
    integer = read <$> many1 digit
    identifier = many1 letter

bexpParser :: Parser Bexp
bexpParser = undefined

-- Define a parser for statements (Stm)
stmParser :: Parser Stm
stmParser = undefined

programParser :: Parser Program
programParser = undefined

parse :: String -> Program
parse input = case Text.ParserCombinators.Parsec.parse programParser "" input of
  Left err -> error $ "Parser error: " ++ show err
  Right program -> program


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run(compile (Main.parse programCode), createEmptyStack, createEmptyState)

testAexpParser :: String -> Either ParseError Aexp
testAexpParser input = Text.ParserCombinators.Parsec.parse aexpParser "" (filter (not . isSpace) input)

-- to test compile and its auxilliary functions
main :: IO ()
main = do
  putStrLn "Part 1 tests:"
  print (testAssembler [Push 10, Push 4, Push 3, Sub, Mult] == ("-10","")) -- works
  print (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")) -- works
  print (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")) -- works
  print (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")) -- works
  print (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")) -- works
  print (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")) -- works
  print (testAssembler [Push (-20),Push (-21), Le] == ("True","")) -- works
  print (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")) -- works
  print (testAssembler [Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")) -- works

  putStrLn "Compile tests:"
  -- arithmetic operations
  putStrLn $ show (testAssembler (compA (AAdd (ANum 2) (ANum 3))) == ("5", [])) -- True
  putStrLn $ show (testAssembler (compA (ASub (ANum 8) (ANum 5))) == ("-3", [])) -- True
  putStrLn $ show (testAssembler (compA (AMul (ANum 4) (ANum 5))) == ("20", [])) -- True

  -- boolean expressions
  putStrLn $ show (testAssembler (compB BTrue) == ("True", [])) -- True
  putStrLn $ show (testAssembler (compB BFalse) == ("False", [])) -- True
  putStrLn $ show (testAssembler (compB (BNot BTrue)) == ("False", [])) -- True
  putStrLn $ show (testAssembler (compB (BAnd BTrue BFalse)) == ("False", [])) -- True
  putStrLn $ show (testAssembler (compB (BLe (ANum 3) (ANum 5))) == ("True", [])) -- True
  putStrLn $ show (testAssembler (compB (BEq (ANum 5) (ANum 5))) == ("True", [])) -- True

  -- variable assignments
  putStrLn $ show (testAssembler (compile [Assign "x" (ANum 42)]) == ("", "x=42")) -- True
  putStrLn $ show (testAssembler (compile [Assign "x" (ANum 42), Assign "y" (AVar "x")]) == ("", "x=42,y=42")) -- True

  -- conditionals
  putStrLn $ show (testAssembler (compile [If BTrue (Assign "x" (ANum 1)) (Assign "y" (ANum 2))]) == ("", "x=1")) -- True
  putStrLn $ show (testAssembler (compile [If BFalse (Assign "x" (ANum 1)) (Assign "y" (ANum 2))]) == ("", "y=2")) -- True

  -- loops
  putStrLn $ show (testAssembler (compile [Assign "i" (ANum 1), While (BLe (AVar "i") (ANum 3)) (Assign "i" (AAdd (AVar "i") (ANum 1)))]) == ("", "i=4")) -- True
  putStrLn $ show (testAssembler (compile [While BFalse (Assign "x" (ANum 1))]) == ("", "")) -- True

  -- combining operations
  putStrLn $ show (testAssembler (compile [Assign "x" (ANum 5), Assign "y" (ANum 3), If (BLe (AVar "x") (AVar "y")) (Assign "z" (ANum 1)) (Assign "z" (ANum 2))]) == ("", "x=5,y=3,z=2")) -- True

  -- lexer tests
  -- putStrLn "Testing lexer:"
  -- print (lexer "x := 5; x := x - 1;")

  -- arithmetic parser tests
  putStrLn "Testing arithmetic parser: (check expected results in comments)"
  putStrLn $ show (testAexpParser "42")                -- Should parse as ANum 42
  putStrLn $ show (testAexpParser "x + 5")             -- Should parse as AAdd (AVar "x") (ANum 5)
  putStrLn $ show (testAexpParser "(x * 5) + y")       -- Should parse as AAdd (AMul (AVar "x") (ANum 5)) (AVar "y")
  putStrLn $ show (testAexpParser "(8 - 5)")           -- Should parse as ASub (AVar "x") (ANum 5)
  putStrLn $ show (testAexpParser "x * (3 + y) - 4")   -- Should parse as ASub (AMul (AVar "x") (AAdd (ANum 3) (AVar "y"))) (ANum 4)
  putStrLn $ show (testAexpParser "8 + (-4)")          -- Should parse as AAdd (ANum 8) (ANum (-4)) - doesn't work yet

  -- general parser tests
  -- putStrLn "Testing parser:"
  -- putStrLn $ show (testParser "x := 5; x := x - 1;" == ("","x=4")) -- True
  -- putStrLn $ show (testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")) -- True
  -- putStrLn $ show (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")) -- True
  -- putStrLn $ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")) -- True
  -- putStrLn $ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")) -- True
  -- putStrLn $ show (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")) -- True
  -- putStrLn $ show (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")) -- True
