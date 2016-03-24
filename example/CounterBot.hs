{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar ( TVar
                                             , newTVarIO
                                             , modifyTVar'
                                             , readTVar
                                             )
import           Control.Monad.Logger ( runStdoutLoggingT
                                      , logInfoNS
                                      , filterLogger
                                      , LogLevel(LevelDebug)
                                      )
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           System.Environment (lookupEnv)

import           Web.Slack


-- Count how many messages the bot receives
counterBot :: TVar Int -> SlackBot
counterBot counter (Message cid _ _ _ _ _) = do
  num <- liftIO $ atomically (do modifyTVar' counter (+ 1)
                                 readTVar counter)
  logInfoNS "counterBot" $ T.pack $ show num
  sendMessage cid (T.pack . show $ num)
counterBot _ _ = return ()

main :: IO ()
main = do
  token <- fromMaybe (error "SLACK_API_TOKEN not set")
           <$> lookupEnv "SLACK_API_TOKEN"
  counter <- newTVarIO 0
  runStdoutLoggingT
    $ filterLogger (\_ lvl -> lvl /= LevelDebug)
    $ runBot (SlackConfig token) (counterBot counter)
