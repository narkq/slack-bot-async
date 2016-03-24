{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Logger ( runStdoutLoggingT
                            , logInfoNS
                            , filterLogger
                            , LogLevel(LevelDebug)
                            )
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import Web.Slack

echoBot :: SlackBot
echoBot (Message cid _ msg _ _ _) = do
  logInfoNS "echoBot" msg
  sendMessage cid msg
echoBot _ = return ()

main :: IO ()
main = do
  token <- fromMaybe (error "SLACK_API_TOKEN not set")
           <$> lookupEnv "SLACK_API_TOKEN"
  runStdoutLoggingT
    $ filterLogger (\_ lvl -> lvl /= LevelDebug)
    $ runBot (SlackConfig token) echoBot
