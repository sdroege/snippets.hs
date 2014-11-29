import Data.Char
import Data.List
import Data.List.Split

unbinary :: String -> Char
unbinary = chr . foldl (\a b -> 2 * a + if b == '1' then 1 else 0) 0

unbinaryString :: String -> String
unbinaryString x = map unbinary (words x)

binary :: Char -> String
binary = tail . reverse . unfoldr (\b -> if b == 0 then Nothing else Just (if b `mod` 2 == 0 then '0' else '1', b `div` 2)) . (+ 256) . ord

binaryString :: String -> String
binaryString = unwords . map binary

main :: IO ()
main = do
  print $ unbinaryString . unwords . chunksOf 8 $ "01111001 01101111 01110101 01110100 01110101 00101110 01100010 01100101 00101111 01100100 01010001 01110111 00110100 01110111 00111001 01010111 01100111 01011000 01100011 01010001"
  print $ binaryString "youtu.be/dQw4w9WgXcQ"

