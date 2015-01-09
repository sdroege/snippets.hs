{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as BU

import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTPS

import qualified Text.XML.Light as XML

import Data.Aeson

import Safe

data OAuth = OAuth
    { oAuthTokenType   :: String
    , oAuthAccessToken :: String
    , oAuthExpiresIn   :: Maybe Int
    , oAuthScope       :: String
    } deriving (Show, Eq)

instance FromJSON OAuth where
    parseJSON (Object v) = OAuth
                            <$> v .: "token_type"
                            <*> v .: "access_token"
                            <*> (readMay <$> (v .: "expires_in"))
                            <*> v .: "scope"
    parseJSON _          = mzero

getXMLString :: String -> Maybe String
getXMLString = fmap XML.strContent . headMay . filter ((stringElementName ==) . XML.elName) . XML.onlyElems . XML.parseXML
    where
        stringElementName = XML.QName "string" (Just "http://schemas.microsoft.com/2003/10/Serialization/") Nothing

main :: IO ()
main = do
    m <- HTTP.newManager HTTPS.tlsManagerSettings

    let url    = "https://datamarket.accesscontrol.windows.net/v2/OAuth2-13"
        scope  = "http://api.microsofttranslator.com"
        client = "XXX"
        secret = "XXX"

    baseReq <- HTTP.parseUrl url
    let req = HTTP.urlEncodedBody [ ("client_id", client)
                                  , ("client_secret", secret)
                                  , ("scope", scope)
                                  , ("grant_type", "client_credentials")
                                  ]
                                  baseReq

    resp <- HTTP.httpLbs req m

    let Just auth = decode (HTTP.responseBody resp) :: Maybe OAuth

    let text = "Hello World!"
        from = "en"
        to   = "de"
        url' = "http://api.microsofttranslator.com/v2/Http.svc/Translate"

    baseReq' <- HTTP.parseUrl url'
    let req' = HTTP.setQueryString [ ("text", Just text)
                                   , ("from", Just from)
                                   , ("to", Just to)
                                   , ("contentType", Just "text/plain")
                                   ]
                                   (baseReq' { HTTP.requestHeaders = ("Authorization", "Bearer " `B.append`
                                                        B8.pack (oAuthAccessToken auth)) : HTTP.requestHeaders baseReq'
                                             }
                                   )

    resp' <- HTTP.httpLbs req' m

    let Just trans = getXMLString . BU.toString . BL.toStrict . HTTP.responseBody $ resp'
    putStrLn trans

