# [slack-bot-async](https://hackage.haskell.org/package/slack-bot-async)

Haskell library for creating [Slack](https://slack.com/) bots
using the Slack [Real Time Messaging API](https://api.slack.com/rtm).
Events are handled asynchronously.

This library uses
[slack-api-types](https://github.com/joehillen/slack-api-types) for API definitions
and [monad-logger](https://hackage.haskell.org/package/monad-logger) for logging.

## Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Logger ( runStdoutLoggingT
                            , logInfoNS
                            , filterLogger
                            , LogLevel(LevelDebug)
                            )
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

import Slack.Bot

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
    $ runBot token echoBot
```

More examples [here](https://github.com/joehillen/slack-bot-async/tree/master/examples).

# Credit

This project is forked from [slack-api](https://hackage.haskell.org/package/slack-api) by Matthew Pickering.
