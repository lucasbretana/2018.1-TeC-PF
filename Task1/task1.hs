{--
 - Given the full text string, 
 - returns the same text but justified
 --}
--justifica :: String -> String
justifica [] = []
justifica s = let list = toList s 
              in (concat ( (map (flip(justLine) (longestString list)) (init list)) ) ) ++ ( last list )
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


-- Example test
texto = "RUBIÃO fitava a enseada -- eram oito horas da manhã.\n\
  \Quem o visse com os polegares metidos no cordão do chambre à janela de uma\n\
  \grande casa de Botafogo cuidaria que ele admirava aquele pedaço de água\n\
  \quieta mas em verdade vos digo que pensava em outra coisa.\n\
  \Cotejava o passado com o presente. Que era há um ano?\n\
  \Professor. Que é agora? Capitalista! Olha para si para as chinelas\n\
  \(umas chinelas de Túnis que lhe deu recente amigo Cristiano Palha) para a casa\n\
  \para o jardim para a enseada para os morros e para o\
  \céu e tudo desde as chinelas\n\
  \até o céu tudo entra na\
  \mesma sensação de propriedade."


-- Examples from Jonathas

textoEx2 = "Jonathas Augusto\nde Oliveira\nConceição"

textoEx3 =
  "\"Lorem ipsum\" text is derived from sections 1.10.33 of Cicero's De finibus bonorum et malorum.[1]\n\
  \\n\
  \It is not known exactly when the text obtained its current standard form; it may have been as late as the 1960s. Dr.\n\
  \Richard McClintock, a Latin scholar who was the\npublications director at Hampden–Sydney College in Virginia, discovered\n\
  \the source of the passage sometime before 1982 while searching for instances of the Latin\n\
  \word \"consectetur\" (\"that [he/she/it] pursue\", subjunctive), rarely used in classical literature.[2][a]\n\
  \The physical source of the lorem ipsum text may be the 1914 Loeb\nClassical Library Edition of the De Finibus,\n\
  \where the Latin text, presented on the left-hand (even) pages, breaks off on page 34 with \"Neque porro quisquam\n\
  \est qui do-\" and continues on page 36 with \"lorem ipsum ...\", suggesting that the galley type of that page was\n\
  \mixed up to make the dummy text seen today.[4]"
