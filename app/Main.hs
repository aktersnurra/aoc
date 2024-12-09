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

addAocCookie :: String -> Request -> Request
addAocCookie aocToken = addRequestHeader hCookie cookie
  where
    cookie = fromString $ "session=" ++ aocToken

addAccept :: Request -> Request
addAccept = addRequestHeader hAccept "text/plain"

addContentType :: Request -> Request
addContentType = addRequestHeader hContentType "application/x-www-form-urlencoded"

-- parse year
-- parse day
-- parse answer

inputRequest :: Request -> Year -> Day -> Request
inputRequest baseRequest year day = do
    let path = fromString $ "/" ++ year ++ "/day/" ++ day ++ "/input"
    setRequestPath path $
        setRequestMethod
            "GET"
            baseRequest

-- TODO: parse html for the response
submitRequest :: Request -> Year -> Day -> Part -> Answer -> Request
submitRequest baseRequest year day part answer = do
    let path = fromString $ "/" ++ year ++ "/day/" ++ day ++ "/answer"
    let content = L8.pack $ "level=" ++ part ++ "&answer=" ++ answer
    setRequestBodyLBS content $
        setRequestPath path $
            setRequestMethod "POST" $
                addAccept $
                    addContentType
                        baseRequest

execute :: Request -> IO ()
execute request = do
    response <- httpLBS request
    L8.putStrLn $ getResponseBody response

main :: IO ()
main = do
    aocToken <- getEnv "AOC_TOKEN"
    let request = setRequestHost "adventofcode.com" $ addUserAgent $ addAocCookie aocToken defaultRequest
    (opts :: Opts) <- execParser optsParser
    case optCommand opts of
        Input year day -> execute $ inputRequest request year day
        Submit year day part answer -> execute $ submitRequest request year day part answer
