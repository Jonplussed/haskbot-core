{-# LANGUAGE OverloadedStrings #-}

module App.Network where

import Data.Text (Text)
import Network.HTTP
import Network.Stream (Result)

data SimpleResponse = Success
                    | Failure String
                    deriving (Eq, Show)

-- constants

typeJSON :: String
typeJSON = "application/json"

-- public functions

sendAsJSON :: String -> String -> IO SimpleResponse
sendAsJSON url content = send $ postRequestWithBody url typeJSON content

-- private function

send :: HStream ty => Request ty -> IO SimpleResponse
send request = do
    resp <- simpleHTTP request
    code <- getResponseCode resp
    reas <- getRespReason resp
    return $ case code of
      (2,0,0) -> Success
      _       -> Failure reas -- this should also log the failure

getRespReason :: Result (Response ty) -> IO String
getRespReason (Left err) = fail $ show err
getRespReason (Right r)  = return $ rspReason r
