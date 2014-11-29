{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import qualified Network.HTTP.Conduit as H
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import qualified Text.HTML.TagStream.Text as CT
import qualified Text.HTML.TagStream.Types as CT
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T

main :: IO ()
main =
    H.withManager $ \manager -> do
        req <- H.parseUrl "http://youtube.com"
        res <- H.http req manager

        title <- H.responseBody res $$+- CT.decodeUtf8Lenient =$= CT.tokenStream =$= getHtml =$= takeNotBody =$= getHead =$= getTitle =$= CC.map (T.unpack . T.strip) =$ CC.fold
        lift $ putStrLn title

        where
            getHtml = dropNonTagOpen "html" =$= takeNonTagClose "html"
            getHead = dropNonTagOpen "head" =$= takeNonTagClose "head"
            takeNotBody = CC.takeWhile (not . matchTagOpenLit "body")
            getTitle = dropNonTagOpen "title" =$= dropC 1 =$= takeTexts =$= CC.map extractText

            dropNonTagOpen :: (Monad m) => T.Text -> Conduit CT.Token m CT.Token
            dropNonTagOpen name = dropWhileC (not . matchTagOpenLit name)

            takeNonTagClose :: (Monad m) => T.Text -> Conduit CT.Token m CT.Token
            takeNonTagClose name = CC.takeWhile (not . matchTagClose name)

            takeTexts :: (Monad m) => Conduit CT.Token m CT.Token
            takeTexts = CC.takeWhile matchTextAny

            dropWhileC :: (Monad m) => (CT.Token -> Bool) -> Conduit CT.Token m CT.Token
            dropWhileC f = CC.dropWhile f >> awaitForever yield

            dropC :: (Monad m) => Int -> Conduit CT.Token m CT.Token
            dropC i = CC.drop i >> awaitForever yield

            matchTagOpenLit :: T.Text -> CT.Token -> Bool
            matchTagOpenLit name tag = case tag of
                                        CT.TagOpen name' _ _ -> name == name'
                                        _                    -> False

            matchTagClose :: T.Text -> CT.Token -> Bool
            matchTagClose name tag = case tag of
                                        CT.TagClose name'    -> name == name'
                                        _                    -> False

            matchTextAny :: CT.Token -> Bool
            matchTextAny tag = case tag of
                                    CT.Text _            -> True
                                    _                    -> False

            extractText tag = case tag of
                                CT.Text t -> t
                                _         -> T.empty

