data E  = Num Int
        | Sum E E
        | Mul E E
  deriving (Show, Eq)

data Ele = Single E | List [E]

data Token = Plus | Times | OPar | CPar | Crap Int | EOF
  deriving(Show, Eq)

parse :: String -> E
parse [] = error "Empty string"


{--
 - Takes the current expressio  list, the string of the remaining expression and the current token
 - Returns a list of expression using sublists for sub expressions
--}
expAsList :: [Ele] -> [String] -> Token -> [Ele]
-- end of expression
expAsList cur [] EOF   = cur
expAsList _   _  EOF   = error "Makes no sense to receive a EOF, but the expression string is not over"
expAsList _   [] Plus  = error "Makes no sense a '+' in the end"
expAsList _   [] Times = error "Makes no sense a '*' in the end"
expAsList _   [] OPar  = error "Makes no sense a '(' in the end"
--expAsList cur [] a    = cur++[a]
-- sub expression
expAsList cur strExp OPar = takeSubExp ['('] 1 str
  where
     {--
     - Simply takes a string, and return any inside parentesis substring
     --}
     takeSubExp :: String -> Int -> String -> String
     takeSubExp subStr 0 _   = subStr
     takeSubExp _      _ []  = error "End of expression string is incorrect. Abort"
     takeSubExp subStr n str@('(':xs) = takeSubExp (subStr++['(']) (n+1) xs
     takeSubExp subStr n str@(')':xs) = takeSubExp (subStr++[')']) (n-1) xs
     takeSubExp subStr n str@(x:xs)   = takeSubExp (subStr++[x]) n xs


--           (input -> (token, remain)
tokenizer :: String -> (Token, String)
tokenizer []  = (EOF, [])
tokenizer str@(x:xs)
  | x == '('  = (OPar, xs)
  | x == '+'  = (Plus, xs)
  | x == ')'  = (CPar, xs)
  | x == '*'  = (Times, xs)
  | otherwise = let (t, s) = readNum str
                  in (Crap t, s)

readNum :: String -> (String, String)
readNum str = (takeWhile (\a -> elem a ['0'..'9']) str, dropWhile (\a -> elem a ['0'..'9']) str)

{--
findMul :: String -> String -> String -> E
findMul str@(x:xs) a b
  | x == '*'  = Mul (parse a) (parse (tail b))
  | otherwise = findMul x:a b (tail b)
--}


exp :: String
exp = "0 + (2 + 20) * 2" -- Sum 0 (Mul (Sum 2 20) 2)
