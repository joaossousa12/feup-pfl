-- PFL 2023/24 - Haskell practical assignment quickstart
module Main where

import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Data.Char (isSpace)
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Foldable (asum)


-- Part 1


-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- definition of StackData type to represent various types of data in the stack
data StackData = Value Integer | Boolean Bool | Expression String
    deriving (Show, Eq)

-- type alias for Stack, which is a list of StackData
type Stack = [StackData]

-- type alias for State, representing storage as a list of key-value pairs
type State = [(String, StackData)]

-- creates empty stack
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
  Push n -> run (tail, Value n : stack, state) -- pushes an integer value onto the stack
  Add -> run (tail, performArithmeticOp (+) stack, state) -- adds the top two integer values on the stack
  Sub -> run (tail, performArithmeticOp (-) stack, state) -- subtracts the top integer value from the second top value on the stack
  Mult -> run (tail, performArithmeticOp (*) stack, state) -- multiplies the top two integer values on the stack
  Tru -> run (tail, Boolean True : stack, state) -- pushes a boolean True onto the stack
  Fals -> run (tail, Boolean False : stack, state) -- pushes a boolean False onto the stack
  Fetch x -> 
    let val = lookup x state in -- fetches the value associated with variable x from the state
      case val of
        Just v -> run (tail, v : stack, state) -- pushes the fetched value onto the stack
        Nothing -> error "Runtime error: Variable not found!"
  Store x -> 
    case stack of
      (v:tailStore) -> run (tail, tailStore, Main.updateState x v state)
      [] -> error "Runtime error: Stack underflow on Store!"
  Neg -> 
    case stack of
      (Boolean b : rest) -> run (tail, Boolean (not b) : rest, state) -- negates the boolean value on top of the stack
      _ -> error "Runtime error: Negation applied to non-boolean value!" 
  Equ -> 
    case stack of
      (x : y : rest) -> run (tail, Boolean (x == y) : rest, state) -- checks equality of the top two stack values and pushes the result
      _ -> error "Runtime error: Insufficient elements on stack for Equ!"
  Le -> 
    case stack of
      (Value x : Value y : rest) -> run (tail, Boolean (x <= y) : rest, state) -- checks if the second top stack value is less or equal to the top value
      _ -> error "Runtime error: Invalid elements on stack for Le or insufficient elements!"
  Noop -> run (tail, stack, state) -- no operation simply continues with the next instruction.
  Branch c1 c2 -> 
    case stack of
      (Boolean True : rest) -> run (c1 ++ tail, rest, state) -- if top stack value is True, continue with c1 code sequence
      (Boolean False : rest) -> run (c2 ++ tail, rest, state) -- if top stack value is False, continue with c2 code sequence
      _ -> error "Runtime error: Non-boolean value on stack for Branch!" -- error if not boolean
  Loop c1 c2 -> 
    let loopExp = c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] in -- constructs a loop expression using Branch
    run (loopExp ++ tail, stack, state) -- executes the loop expression
  And -> 
    case stack of
      (Boolean x : Boolean y : rest) -> run (tail, Boolean (x && y) : rest, state) -- performs logical AND on the top two boolean stack values
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


-- Part 2


-- TYPES

-- arithmetic expressions
data Aexp
  = ANum Integer          -- represents a numeric constant
  | AVar String           -- represents a variable reference
  | AAdd Aexp Aexp        -- represents an addition expression
  | ASub Aexp Aexp        -- represents a subtraction expression
  | AMul Aexp Aexp        -- represents a multiplication expression
  deriving (Show)

-- boolean expressions
data Bexp
  = BTrue                 -- represents a boolean True
  | BFalse                -- represents a boolean False
  | BNot Bexp             -- represents a logical NOT operation
  | BAnd Bexp Bexp        -- represents a logical AND operation
  | BLe Aexp Aexp         -- represents a less than or equal to comparison
  | BEq Aexp Aexp         -- represents an equality comparison (boolean or arithmetic because "=" is missing)
  deriving (Show)

-- statements
data Stm
  = Assign String Aexp    -- represents a variable assignment statement
  | Seq [Stm]             -- represents a sequence of statements
  | If Bexp Stm Stm       -- represents a conditional statement
  | While Bexp Stm        -- represents a while loop statement
  deriving (Show)

type Program = [Stm]      -- represents a program as a list of statements


-- COMPILER

-- compA function compiles arithmetic expressions into code
compA :: Aexp -> Code
compA (ANum n) = [Push n]
compA (AVar x) = [Fetch x]
compA (AAdd e1 e2) = compA e1 ++ compA e2 ++ [Add]
compA (ASub e1 e2) = compA e1 ++ compA e2 ++ [Sub]
compA (AMul e1 e2) = compA e1 ++ compA e2 ++ [Mult]

-- compB function compiles boolean expressions into code
compB :: Bexp -> Code
compB (BLe e1 e2) = compA e2 ++ compA e1 ++ [Le]
compB (BEq e1 e2) = compA e2 ++ compA e1 ++ [Equ]
compB (BAnd b1 b2) = compB b1 ++ compB b2 ++ [And]
compB (BNot b) = compB b ++ [Neg]
compB BTrue = [Tru]
compB BFalse = [Fals]

-- compile function translates a program into a sequence of instructions
compile :: Program -> Code
compile [] = []
compile (stmt : rest) = case stmt of
  Assign x aexp -> compA aexp ++ [Store x] ++ compile rest
  Seq stmts -> concatMap compile [stmts] ++ compile rest
  If bexp stm1 stm2 -> compB bexp ++ [Branch (compile [stm1]) (compile [stm2])] ++ compile rest
  While bexp stm -> [Loop (compB bexp) (compile [stm])] ++ compile rest

-- PARSER

-- handle parenthesis
parens :: Parser a -> Parser a
parens p = between (char '(' <* spaces) (char ')' <* spaces) p

-- format_input function breaks input into tokens, separated by semicolons
format_input :: String -> [String]
format_input "" = []
format_input input =
  let (token, rest) = breakDelimiter input
  in token : case rest of
    [] -> []
    xs -> format_input xs

-- breakDelimiter function breaks input at semicolons
breakDelimiter :: String -> (String, String)
breakDelimiter "" = ("", "")
breakDelimiter (';' : 'e' : 'l' : 's' : 'e' : xs) =
  let (token, rest) = breakDelimiter xs
  in (";else" ++ token, rest)
breakDelimiter (';' : xs) = ("", xs)
breakDelimiter (x : xs) = (x : token, rest)
  where
    (token, rest) = breakDelimiter xs

-- aexpParser function parses arithmetic expressions.
-- It uses buildExpressionParser to handle operator precedence and associativity.
-- It recognizes numeric constants, variable references, and supports addition,
-- subtraction, and multiplication operations. It also correctly handles parentheses.
aexpParser :: Parser Aexp
aexpParser = buildExpressionParser operators term <?> "expression"
  where
    operators = [ [Infix (AMul <$ spaces <* char '*' <* spaces) AssocLeft]
                , [Infix (flipASub <$ spaces <* char '-' <* spaces) AssocLeft
                  , Infix (AAdd <$ spaces <* char '+' <* spaces) AssocLeft] ]
    term = try (ANum <$> (char '-' *> integer <|> integer))
       <|> try (AVar <$> identifier)
       <|> parens aexpParser
    integer = read <$> many1 digit
    identifier = many1 letter

    flipASub :: Aexp -> Aexp -> Aexp
    flipASub = flip ASub

-- bexpParser function parses boolean expressions.
-- It recognizes boolean constants (True and False), logical NOT, and AND operations.
-- It uses buildExpressionParser to handle operator precedence and associativity.
-- It also parses relational expressions for less than or equal to and equality checks (missing "="!)
bexpParser :: Parser Bexp
bexpParser = buildExpressionParser operators term <?> "boolean expression"
  where
    operators = [ [Prefix (BNot <$ spaces <* string "not" <* spaces)]
                , [Infix (BAnd <$ spaces <* string "and" <* spaces) AssocLeft]
                ]
    term = try (parens bexpParser)
      <|> try (BTrue <$ string "true")
      <|> try (BFalse <$ string "false")
      <|> relExpParser
    relExpParser = try (BLe <$> aexpParser <* spaces <* string "<=" <* spaces <*> aexpParser)
      <|> (BEq <$> aexpParser <* spaces <* string "==" <* spaces <*> aexpParser)

-- stmParser function parses statements.
-- It combines parsers for variable assignment, conditional statements (if-else),
-- and while loops. It ensures correct parsing of statements with proper syntax,
-- including handling variable assignments, if conditions, and loop structures.
stmParser :: Parser Stm
stmParser = assignParser <|> ifParser <|> whileParser
  where
    assignParser :: Parser Stm
    assignParser = do
      variable <- notFollowedBy (choice $ map string reservedKeywords) *> many1 letter <* spaces <* string ":=" <* spaces
      expression <- aexpParser
      return (Assign variable expression)

    ifParser :: Parser Stm
    ifParser = do
      string "if"
      optional spaces
      condition <- try bexpParser
      optional spaces
      string "then"
      optional spaces
      trueBranch <- stmParser <* optional (char ';')
      optional spaces
      string "else"
      optional spaces
      falseBranch <- stmParser
      return (If condition trueBranch falseBranch)

    whileParser :: Parser Stm
    whileParser = do
      string "while" *> spaces
      condition <- parens bexpParser <* spaces
      string "do" *> spaces
      body <- parens (stmParser <* char ';') <* char ';'
      return (While condition body)

    reservedKeywords = ["if", "then", "else", "while", "do", "not", "true", "false"]

parseStatement :: String -> Stm
parseStatement input = case Text.ParserCombinators.Parsec.parse stmParser "" input of
  Left err -> error $ "Parse error: " ++ show err
  Right statement -> statement

parse :: String -> Program
parse input = map parseStatement (format_input (filter (not . isSpace) input))

-- to run tests
main :: IO ()
main = do
  putStrLn "\n\nPart 1 tests:\n"
  print (testAssembler [Push 10, Push 4, Push 3, Sub, Mult] == ("-10","")) -- works
  print (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")) -- works
  print (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")) -- works
  print (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")) -- works
  print (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")) -- works
  print (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")) -- works
  print (testAssembler [Push (-20),Push (-21), Le] == ("True","")) -- works
  print (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")) -- works
  print (testAssembler [Push 10, Store "i", Push 1, Store "fact", Loop [Push 1, Fetch "i", Equ, Neg] [Fetch "i", Fetch "fact", Mult, Store "fact", Push 1, Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")) -- works

  putStrLn "\n\nCompile tests:\n"
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

  -- arithmetic parser tests
  putStrLn "\n\nTesting arithmetic parser:\n"
  putStrLn $ show (testAexpParser "42")                            -- ANum 42
  putStrLn $ show (testAexpParser "x + 5")                         -- AAdd (AVar "x") (ANum 5)
  putStrLn $ show (testAexpParser "(x * 5) + y")                   -- AAdd (AMul (AVar "x") (ANum 5)) (AVar "y")
  putStrLn $ show (testAexpParser "(8 - 5)")                       -- ASub (AVar "x") (ANum 5)
  putStrLn $ show (testAexpParser "x * (3 + y) - 4")               -- ASub (AMul (AVar "x") (AAdd (ANum 3) (AVar "y"))) (ANum 4)
  putStrLn $ show (testAexpParser "8 + (-4)")                      -- AAdd (ANum 8) (ANum (-4)) - doesn't work yet *******************

  -- boolean expression parser tests
  putStrLn "\n\nTesting boolean expression parser:\n"
  putStrLn $ show (testBexpParser "true;")                         -- BTrue
  putStrLn $ show (testBexpParser "false;")                        -- BFalse
  putStrLn $ show (testBexpParser "true and false;")               -- BAnd (BTrue BFalse)
  putStrLn $ show (testBexpParser "(true and false);")             -- BAnd (BTrue BFalse)
  putStrLn $ show (testBexpParser "(true and false) and true;")    -- BAnd (BAnd BTrue BFalse) BTrue
  putStrLn $ show (testBexpParser "not false;")                    -- BNot BFalse
  putStrLn $ show (testBexpParser "not (true and false);")         -- BNot (BAnd BTrue BFalse)
  putStrLn $ show (testBexpParser "not true and false;")           -- BNot (BAnd BTrue BFalse)
  putStrLn $ show (testBexpParser "1 <= 2")                        -- BLe (ANum 1) (ANum 2)
  putStrLn $ show (testBexpParser "(1 <= 2);")                     -- BLe (ANum 1) (ANum 2)
  putStrLn $ show (testBexpParser "(x <= 5) and not (y <= 5);")    -- BAnd (BLe (AVar "x") (ANum 5)) (BNot (BLe (AVar "y") (ANum 5)))
  putStrLn $ show (testBexpParser "2 <= 5 and 3 == 4;")            -- BAnd (BLe (ANum 2) (ANum 5)) (BEq (ANum 3) (ANum 4))

  -- statement parser tests
  putStrLn "\n\nTesting statement parser:\n"
  putStrLn $ show (testStmParser "x := 42;")                                 -- Assign "x" (ANum 42)
  putStrLn $ show (testStmParser "x := 42;")                                 -- Assign "x" (ANum 42)
  putStrLn $ show (testStmParser "y := x + 5;")                              -- Assign "y" (AAdd (AVar "x") (ANum 5))
  putStrLn $ show (testStmParser "z := (x * 5) + y;")                        -- Assign "z" (AAdd (AMul (AVar "x") (ANum 5)) (AVar "y"))
  putStrLn $ show (testStmParser "if true then x := 1; else x := 33")        -- (If BTrue (Assign "x" (ANum 1)) (Assign "x" (ANum 33)))
  putStrLn $ show (testStmParser "if x <= 43 then x := 1; else x := 33;")    -- (If (BLe (AVar "x") (ANum 43)) (Assign "x" (ANum 1)) (Assign "x" (ANum 33)))
  putStrLn $ show (testStmParser "while (not(i == 1)) do (i := 1;);")        -- (While (BNot (BEq (AVar "i") (ANum 1))) (Assign "i" (ANum 1)))

  -- general parser tests
  putStrLn "\n\nTesting parser:\n"
  putStrLn $ show (testParser "x := 4;" == ("","x=4")) -- works
  putStrLn $ show (testParser "x := 2; y := 5; z := (x * 5) + y;" == ("","x=2,y=5,z=15")) -- works
  putStrLn $ show (testParser "x := 3; if (x == 2) then x := 1 else x := 33;" == ("","x=33")) -- works
  putStrLn $ show (testParser "if not (2 <= 5) then x := 1 else y := 2" == ("","y=2")) -- works
  -- putStrLn $ show (testParser "if (not True and 2 <= 5 = 3 == 3) then x :=1 else y := 2" == ("","x=1")) -- doesn't work
  -- putStrLn $ show (testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")) -- doesn't work - ifs e whiles com mais de um statement não funcionam
  -- putStrLn $ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")) -- doesn't work
  putStrLn $ show (testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")) -- True
  putStrLn $ show (testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6") ) -- True
  -- putStrLn $ show (testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")) -- doesn't work - como nos ifs, whiles com mais de um statement não funcionam

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_, stack, state) = run(compile (Main.parse (filter (not . isSpace) programCode)), createEmptyStack, createEmptyState)

testAexpParser :: String -> Either ParseError Aexp
testAexpParser input = Text.ParserCombinators.Parsec.parse aexpParser "" (filter (not . isSpace) input)

testBexpParser :: String -> Either ParseError Bexp
testBexpParser input = Text.ParserCombinators.Parsec.parse bexpParser "" (filter (not . isSpace) input)

testStmParser :: String -> Either ParseError Stm
testStmParser input = Text.ParserCombinators.Parsec.parse stmParser "" (filter (not . isSpace) input)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)