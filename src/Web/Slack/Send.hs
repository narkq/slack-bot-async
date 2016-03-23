{-# LANGUAGE TemplateHaskell #-}
module Web.Slack.Send where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue (writeTQueue)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (asks)
import           Data.Text (Text)

import           Web.Slack.Types.Id (ChannelId)
import           Web.Slack.Types.SlackBotSession (SlackBotSession(..))
import           Web.Slack.Types.Slack (Slack)
import           Web.Slack.Types.SlackSendQueue (SendPayload(..))


-- | Send a message to the specified channel.
--
-- If the message is longer than 4000 bytes then the connection will be
-- closed.
sendMessage :: ChannelId -> Text -> Slack ()
sendMessage cid message = do
  queue <- asks _sendQueue
  liftIO . atomically . writeTQueue queue $ SendMessage cid message


-- | Send a ping packet to the server
-- The server will respond with a @pong@ `Event`.
ping :: Slack ()
ping = do
  queue <- asks _sendQueue
  liftIO . atomically $ writeTQueue queue SendPing
