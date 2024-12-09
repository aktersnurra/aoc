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

addUserAgent :: Request -> Request
addUserAgent = addRequestHeader hUserAgent "gustaf+aoc@gustafrydholm.xyz"

addAocCookie :: AocToken -> Request -> Request
addAocCookie token = addRequestHeader hCookie cookie
 where
  cookie = fromString $ "session=" ++ token

addAccept :: Request -> Request
addAccept = addRequestHeader hAccept "text/plain"

addContentType :: Request -> Request
addContentType = addRequestHeader hContentType "application/x-www-form-urlencoded"

aocDefaultRequest :: AocToken -> Request
aocDefaultRequest token = setRequestHost "adventofcode.com" $ addUserAgent $ addAocCookie token defaultRequest

-- parse year
-- parse day
-- parse answer

inputRequest :: Request -> Year -> Day -> Request
inputRequest baseRequest year day = do
  setRequestPath path $
    setRequestMethod
      "GET"
      baseRequest
 where
  path = fromString $ "/" ++ year ++ "/day/" ++ day ++ "/input"

-- TODO: parse html for the response
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
  L8.putStrLn $ getResponseBody response

main :: IO ()
main = do
  token <- getEnv "AOC_TOKEN"
  let request = setRequestHost "adventofcode.com" $ addUserAgent $ addAocCookie token defaultRequest
  (opts :: Opts) <- execParser optsParser
  case optCommand opts of
    Input year day -> execute $ inputRequest request year day
    Submit year day part answer -> execute $ submitRequest request year day part answer
