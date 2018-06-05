{--
 - Given the full text string, 
 - returns the same text but justified
 --}
--justifica :: String -> String
justifica [] = []
justifica s = let list = toList s 
              in concat $ map (flip(justLine) (longestString list)) list
--justifica _ = error "To implement"

{--
 - Given a line and the max size, 
 - returns the same line but jutified
 --}
justLine :: String -> Int -> String
justLine [] _ = []
justLine s m = jLine s (spaces m (length s) (count s ' '))
  where 
    -- given the max size, the current size, and number of spaces, ...
    spaces :: Int -> Int -> Int -> (Int, Int)
    spaces m l 0 = (0, 0)
    spaces m l s = (div (m - l) s, mod (m - l) s)
    -- given the string, the N of spaces, and the R of spaces, ...
    jLine :: String -> (Int, Int) -> String
    jLine [] (_, 0)       = ['\n']
    jLine (' ':xs) (n, 0) = (replicate (n+1) ' ')     ++ (jLine xs (n, 0))
    jLine (' ':xs) (n, r) = (replicate (n+2) ' ') ++ (jLine xs (n, (r-1)))
    jLine (x:xs) (n, r)   = x : (jLine xs (n, r))


{--
 - Given a list an one element, 
 - returns the number of times that element
 - appears on the list
 --}
count :: Eq a => [a] -> a -> Int
count [] _ = 0
count (x:xs) y
  | x == y    = 1 + ( count xs y )
  | otherwise = count xs y


{--
 - Given the full text string, 
 - returns the lines in a list of strings
 --}
toList :: String -> [String]
toList [] = error "Empty string"
toList s  = inList s []
  where
  inList :: String -> String -> [String]
  inList [] s         = [s]
  inList ('\n':xs) s  = s:(inList xs [])
  inList (x:xs) s     = inList xs (s++[x])

{--
 - Given a list of string, 
 - returns the size of the biggest one
 --}
longestString :: [String] -> Int
longestString []   = 0
longestString (x:xs)
  | (length x) > o = length x
  | otherwise      = o
  where
    o = longestString xs

