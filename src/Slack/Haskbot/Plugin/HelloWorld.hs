{-# LANGUAGE OverloadedStrings #-}

module Plugin.HelloWorld (register) where

import Slack.Haskbot.Plugin

name :: NameStr
name = "hello_world"

helpText :: HelpStr
helpText = "Say _Hello, World!_ in your current channel via `/hello_world`."

handler :: HandlerFn
handler slashCom = return $ replySameChan slashCom "Hello, World!"

register :: TokenStr -> Plugin
register = newPlugin name helpText handler
