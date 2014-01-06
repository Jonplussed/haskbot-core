module Slack.Responses (responseFor) where

import qualified Slack.Message as S

import Happstack.Server
  ( Response
  , ServerPart
  , toResponse
  , ok
  )

--
-- public functions
--

responseFor :: S.SlackMsg -> ServerPart Response
responseFor msg = ok . toResponse $ S.user msg ++ ": " ++ S.text msg

--
-- private functions
--
