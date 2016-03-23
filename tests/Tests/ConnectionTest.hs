module Tests.ConnectionTest (main) where

import Web.Slack
import System.Environment (lookupEnv)
import System.Exit
import System.IO.Unsafe

myConfig :: String -> SlackConfig
myConfig apiToken = SlackConfig
         { _slackApiToken = apiToken -- Specify your API token here
         }

connectBot :: SlackBot
connectBot Hello = unsafePerformIO exitSuccess
connectBot _ = return ()


main :: IO ()
main = do
  apiToken <- lookupEnv "SLACK_API_TOKEN"
  case apiToken of
    Nothing -> error "Environment variable 'SLACK_API_TOKEN' not found!"
    Just token -> runBot (myConfig token) connectBot
