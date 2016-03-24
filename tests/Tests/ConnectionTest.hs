{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.ConnectionTest (main) where

import Control.Monad.Logger (runStdoutLoggingT, logInfoS)
import System.Environment (lookupEnv)

import Web.Slack

-- Hack to make it possible to exit the test program from a thread.
foreign import ccall "exit" exit :: Int -> IO ()

connectBot :: SlackBot
connectBot Hello = do
  $logInfoS "connectBot" "Got 'Hello'"
  liftIO $ exit 0
connectBot _ = return ()


main :: IO ()
main = do
  apiToken <- lookupEnv "SLACK_API_TOKEN"
  case apiToken of
    Nothing -> error "Environment variable 'SLACK_API_TOKEN' not found!"
    Just token -> runStdoutLoggingT $ runBot (SlackConfig token) connectBot
