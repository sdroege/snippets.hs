import Text.XML.HXT.Core

import Data.List (intercalate)
import System.IO.Error (isEOFError)
import Control.Exception (try, throw)

main :: IO ()
main = do
        content <- readStdin
        let title = runLA (hreadDoc >>> getTitle) content
        putStrLn $ intercalate "\n" title
        return ()
    where
        getTitle = hasName "html" /> hasName "head" /> hasName "title" /> getText

        readStdin :: IO String
        readStdin = readStdin' []
            where
                readStdin' :: [String] -> IO String
                readStdin' ls = do
                    eitherLine <- try getLine
                    case eitherLine of
                        Left e | isEOFError e   -> return $ (concat . reverse) ls
                        Left e                  -> throw e
                        Right l                 -> readStdin' (l : ls)

