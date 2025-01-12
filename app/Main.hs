{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import CLI
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.ByteString.UTF8 (fromString)
import Network.HTTP.Simple
import Network.HTTP.Types (hAccept, hContentType, hCookie, hUserAgent)
import Options.Applicative
import System.Environment (getEnv)

type AocToken = String
type UserAgent = String

addUserAgent :: UserAgent -> Request -> Request
addUserAgent userAgent = addRequestHeader hUserAgent (read userAgent)

addAocCookie :: AocToken -> Request -> Request
addAocCookie token = addRequestHeader hCookie cookie
 where
  cookie = fromString $ "session=" ++ token

addAccept :: Request -> Request
addAccept = addRequestHeader hAccept "text/plain"

addContentType :: Request -> Request
addContentType = addRequestHeader hContentType "application/x-www-form-urlencoded"

aocDefaultRequest :: AocToken -> UserAgent -> Request
aocDefaultRequest token userAgent =
  setRequestHost "adventofcode.com" $
    addUserAgent userAgent $
      addAocCookie token defaultRequest

inputRequest :: Request -> Year -> Day -> Request
inputRequest baseRequest year day = do
  setRequestPath path $
    setRequestMethod
      "GET"
      baseRequest
 where
  path = fromString $ "/" ++ year ++ "/day/" ++ day ++ "/input"

submitRequest :: Request -> Year -> Day -> Part -> Answer -> Request
submitRequest baseRequest year day part answer = do
  setRequestBodyLBS content $
    setRequestPath path $
      setRequestMethod "POST" $
        addAccept $
          addContentType
            baseRequest
 where
  path = fromString $ "/" ++ year ++ "/day/" ++ day ++ "/answer"
  content = L8.pack $ "level=" ++ part ++ "&answer=" ++ answer

execute :: Request -> IO ()
execute request = do
  response <- httpLBS request
  L8.putStr . getResponseBody $ response

main :: IO ()
main = do
  token <- getEnv "AOC_TOKEN"
  userAgent <- getEnv "AOC_USER_AGENT"
  let request = aocDefaultRequest token userAgent
  (opts :: Opts) <- execParser optsParser
  case optCommand opts of
    Input year day -> execute $ inputRequest request year day
    Submit year day part answer -> execute $ submitRequest request year day part answer
