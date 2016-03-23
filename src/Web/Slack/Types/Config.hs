{-# LANGUAGE TemplateHaskell #-}
module Web.Slack.Types.Config  where

import Control.Lens.TH (makeLenses)

-- | Configuration options needed to connect to the Slack API
data SlackConfig = SlackConfig
                 { _slackApiToken :: String -- ^ API Token for Bot
                 } deriving (Show)

makeLenses ''SlackConfig
