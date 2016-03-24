{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Slack.Types.Slack where

import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.Logger (MonadLogger, LoggingT, runLoggingT)

import Web.Slack.Types.SlackBotSession (SlackBotSession, logger)

newtype Slack a = Slack {unSlack :: ReaderT SlackBotSession (LoggingT IO) a}
  deriving ( Monad, Functor, Applicative, MonadIO
           , MonadReader SlackBotSession
           , MonadLogger
           )

runSlackBot :: SlackBotSession -> Slack () -> IO ()
runSlackBot session = (`runLoggingT` (session ^. logger))
                    . (`runReaderT` session)
                    . unSlack
