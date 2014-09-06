{-# LANGUAGE OverloadedStrings #-}

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB
import Data.String.Utils (strip)

import Safe

import System.IO (stdin)

import Control.Applicative

main :: IO ()
main = do
        content <- readStdin
        let tags = parseTags content
        let title = getTitle tags
        case title of
            Just s  -> putStrLn s
            Nothing -> putStrLn "no title found"

    where
        getTitle = fmap strip . headDef Nothing . fmap maybeTagText . getTitleBlock . getHeadBlock . getHtmlBlock
        getBlock name = takeWhile (not . tagCloseNameLit name) . drop 1 . dropWhile (not . tagOpenNameLit name)
        getHtmlBlock = getBlock "html"
        getHeadBlock = getBlock "head"
        getTitleBlock = getBlock "title"
        readStdin :: IO String
        readStdin = UB.toString <$> B.hGetContents stdin

