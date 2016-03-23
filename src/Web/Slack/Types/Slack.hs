{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Slack.Types.Slack where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)

import Web.Slack.Types.SlackBotSession (SlackBotSession)

newtype Slack a = Slack {runSlack :: ReaderT SlackBotSession IO a}
  deriving ( Monad, Functor, Applicative, MonadIO
           , MonadReader SlackBotSession
           )
