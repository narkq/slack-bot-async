{-# LANGUAGE TemplateHaskell #-}
module Web.Slack.Types.Payload where

import Data.Aeson.TH
import Data.Char (toLower)
import Data.Text (Text)

import Web.Slack.Types.Id (ChannelId)

data MessagePayload = MessagePayload
                    { messageId      :: Int
                    , messageType    :: Text
                    , messageChannel :: ChannelId
                    , messageText    :: Text
                    } deriving Show

$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 7} ''MessagePayload)

messagePayload :: Int -> ChannelId -> Text -> MessagePayload
messagePayload uid = MessagePayload uid "message"


data PingPayload = PingPayload
                 { pingId        :: Int
                 , pingType      :: Text
                 , pingTimestamp :: Int
                 } deriving Show

$(deriveToJSON defaultOptions {fieldLabelModifier = map toLower . drop 4} ''PingPayload)

pingPayload :: Int -> Int -> PingPayload
pingPayload uid = PingPayload uid "ping"
