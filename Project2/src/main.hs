import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

-- PFL 2023/24 - Haskell practical assignment quickstart

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

-- creates empty state
createEmptyState :: State
createEmptyState = []

-- state2Str works by:
-- 1. Sorting the state by variable names (sorted order for better readability)
-- 2. Mapping each key-value pair to its string representation using pairToStr
-- 3. Concatenating the resulting list of strings with commas
state2Str :: State -> String
state2Str state = intercalate "," $ map pairToStr $ sortBy (comparing fst) state
  where
    pairToStr :: (String, StackData) -> String
    pairToStr (var, val) = var ++ "=" ++ stackDataToStr val

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) -- base case with empty code list
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
      (v:tailStore) -> run (tail, tailStore, updateState x v state) -- pops the top value from the stack and stores it in the state with variable x
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
updateState :: String -> StackData -> State -> State
updateState var val [] = [(var, val)]
updateState var val ((v, _):tail) | v == var = (var, val) : tail
updateState var val (pair:tail) = pair : updateState var val tail

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Testing:
-- main :: IO()
-- main = do
--   putStrLn "Part 1:"
--   print (testAssembler [Push 10, Push 4, Push 3, Sub, Mult] == ("-10","")) -- works
--   print (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")) -- works
--   print (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")) -- works
--   print (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")) -- works
--   print (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")) -- works
--   print (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")) -- works
--   print (testAssembler [Push (-20),Push (-21), Le] == ("True","")) -- works
--   print (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")) -- works
--   print (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")) -- works

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
