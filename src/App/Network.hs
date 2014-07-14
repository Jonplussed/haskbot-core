{-# LANGUAGE OverloadedStrings #-}

module App.Network where

import Data.Text (Text)

-- constants

typeJSON :: Text
typeJSON = "application/json"

-- public functions

sendAsJSON :: Text -> Text -> IO ()
sendAsJSON url content = return ()
