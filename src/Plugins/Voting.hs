module Voting
( tallyVoteFor
, newVote
) where

import Database.Redis

import Parser.Commons (Plugin, commandWithArgs)

-- constants

defaultChoices :: [String]
defaultChoices  = ["yes", "no"]

-- public functions

tallyVoteFor :: User -> Plugin
tallyVoteFor user = 

newVote :: Plugin

-- private functions
