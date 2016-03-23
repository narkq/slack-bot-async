{-# LANGUAGE ForeignFunctionInterface #-}
module Tests.ConnectionTest (main) where

import Web.Slack
import System.Environment (lookupEnv)

-- Hack to make it possible to exit the test program from a thread.
foreign import ccall "exit" exit :: Int -> IO ()

connectBot :: SlackBot
connectBot Hello = liftIO $ exit 0
connectBot _ = return ()


main :: IO ()
main = do
  apiToken <- lookupEnv "SLACK_API_TOKEN"
  case apiToken of
    Nothing -> error "Environment variable 'SLACK_API_TOKEN' not found!"
    Just token -> runBot (SlackConfig token) connectBot
