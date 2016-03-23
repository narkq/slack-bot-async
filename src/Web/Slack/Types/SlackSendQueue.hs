module Web.Slack.Types.SlackSendQueue where

import Control.Concurrent.STM.TQueue (TQueue)
import Data.Text (Text)

import Web.Slack.Types.Id (ChannelId)

data SendPayload = SendPing
                 | SendMessage ChannelId Text

type SlackSendQueue = TQueue SendPayload
