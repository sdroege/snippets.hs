{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Network
import Data.Default
import Control.Monad
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLSE
import qualified Crypto.Random.AESCtr as RNG
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UB
import System.X509 (getSystemCertificateStore)

main :: IO ()
main = do
    let server = "www.google.com"
        port   = 443::Int
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    hSetEncoding h utf8
    cs <- getSystemCertificateStore
    let params = (TLS.defaultParamsClient server B.empty) { TLS.clientSupported=def { TLS.supportedCiphers = TLSE.ciphersuite_medium }, TLS.clientShared=def { TLS.sharedCAStore = cs } }
    rnd <- RNG.makeSystem
    ctx <- TLS.contextNew h params rnd
    TLS.handshake ctx
    TLS.sendData ctx "GET /\r\n"
    forever $ do
        d <- TLS.recvData ctx
        putStrLn (UB.toString d)

