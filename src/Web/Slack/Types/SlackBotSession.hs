{-# LANGUAGE TemplateHaskell #-}
module Web.Slack.Types.SlackBotSession where

import Control.Lens (makeLenses)
import Control.Monad.Logger (Loc, LogSource, LogLevel, LogStr)
import qualified Network.WebSockets         as WS

import Web.Slack.Types.Session (SlackSession)
import Web.Slack.Types.Config (SlackConfig)
import Web.Slack.Types.SlackSendQueue (SlackSendQueue)

data SlackBotSession = SlackBotSession
  { _sendQueue    :: SlackSendQueue
  , _slackSession :: SlackSession -- ^ Session information from the
                                  -- start of the connection
  , _config       :: SlackConfig  -- ^ A copy of the initial configuration
  , _connection   :: WS.Connection
  , _logger       :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }


makeLenses ''SlackBotSession
