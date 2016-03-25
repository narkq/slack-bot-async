{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Tests.ConnectionTest where

import Control.Monad.Logger (runStdoutLoggingT, logInfoS)
import System.Environment (lookupEnv)

import Slack.Bot

-- Hack to make it possible to exit the test program from a thread.
foreign import ccall "exit" exit :: Int -> IO ()

connectBot :: SlackBot
connectBot Hello = do
  $logInfoS "connectBot" "Got 'Hello'"
  liftIO $ exit 0
connectBot _ = return ()


connectTest :: IO ()
connectTest = do
  lookupEnv "SLACK_API_TOKEN" >>= \case
    Nothing -> error "Environment variable 'SLACK_API_TOKEN' not found!"
    Just token -> runStdoutLoggingT $ runBot token connectBot
