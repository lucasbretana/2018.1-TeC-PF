data E  = Num Int
        | Sum E E
        | Mul E E
  deriving (Show, Eq)

parse :: String -> E
parse [] = error "Empty string"
parse str = findMul str [] str


findMul :: String -> String -> String -> E
findMul str@(x:xs) a b
  | x == '*'  = Mul (parse a) (parse (tail b))
  | otherwise = findMul x:a b (tail b)


exp :: String
exp = "0 + (2 + 20) * 2"
