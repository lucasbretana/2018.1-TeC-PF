import Data.List.Index

data E  = Num Int
        | Sum E E
        | Mul E E
  deriving (Show, Eq)

data Token = Plus | Times | OPar | CPar | N Int | EOF
  deriving(Show, Eq)

parse :: String -> E
parse []          = error "Empty string"
--parse str@(x:xs)  = 

divideOp :: [(Int, Token)] -> 

expAsList :: String -> [Token]
expAsList [] = EOF:[]
expAsList strExp@('+':xs) = Plus:(expAsList xs)
expAsList strExp@('*':xs) = Times:(expAsList xs)
expAsList strExp@('(':xs) = OPar:(expAsList xs)
expAsList strExp@(')':xs) = CPar:(expAsList xs)
expAsList str             = num:(expAsList rem)
                                  where (num, rem) = readNum str

readNum :: String -> (Token, String)
readNum sExp = ( N (read ( takeWhile (\a -> elem a ['0'..'9']) sExp )::Int), dropWhile (\b -> elem b ['0'..'9']) sExp )
