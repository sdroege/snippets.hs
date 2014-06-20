import System.Random

leftBeaks, rightBeaks :: [String]
leftBody, rightBody :: [String]
heads :: [String]
leftBeaks  = [">", "="]
rightBeaks = ["<", "="]
leftBody   = ["_/", "__/", "_~", "__~"]
rightBody  = ["\\_", "\\__", "~_", "~__"]
heads      = ["o", "O", "0", "@", "©", "®", "ð", "*", "ò", "ô", "ó", "ø", "⊕", "Ω", "ꙫ", "ꙩ", "Ꙩ", "ȯ", "◔", "õ", "ȯ", "⁰", "Ö", "Ó", "Ò", "Õ", "Ô", "ö"]

ducks :: [String]
ducks = [be ++ h ++ bo| be <- leftBeaks, bo <- leftBody, h <- heads]
        ++
        [bo ++ h ++ be| be <- rightBeaks, bo <- rightBody, h <- heads]

duck :: IO String
duck = do
          idx <- randomRIO (0, pred $ length ducks)
          return $ ducks !! idx

main :: IO ()
main = do
          d <- duck
          putStrLn d
          putStrLn allDucksStr
       where
          allDucksStr = foldl1 (\a b -> a ++ " " ++ b) ducks

