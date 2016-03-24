{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
------------------------------------------
-- |
-- This module exposes functionality to write bots which responds
-- to `Event`s sent by the RTM API. By using the user state parameter `s`
-- complicated interactions can be established.
--
-- This basic example echos every message the bot recieves.
-- Other examples can be found in the
-- FIXME
-- @<http://google.com examples>@ directory.
--
-- > myConfig :: SlackConfig
-- > myConfig = SlackConfig
-- >         { _slackApiToken = "..." -- Specify your API token here
-- >         }
-- >
-- > -- type SlackBot = Event -> Slack ()
-- > echoBot :: SlackBot
-- > echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
-- > echoBot _ = return ()
-- >
-- > main :: IO ()
-- > main = runBot myConfig echoBot ()
--
module Web.Slack
( runBot
-- Re-exports
, Slack(..)
, SlackBot
, liftIO
, ping
, sendMessage
, module Web.Slack.Types
) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue ( readTQueue
                                               , newTQueueIO
                                               )
import           Control.Lens
import           Control.Monad (forever, void, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (ask)
import           Control.Monad.Logger ( LoggingT
                                      , logDebugNS
                                      , logInfoN
                                      , askLoggerIO
                                      , logErrorN
                                      , logErrorNS
                                      )
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Monoid ((<>))
import qualified Data.Text                  as T
import           Data.Text.Lens (unpacked)
import qualified Network.Socket             as S
import qualified Network.URI                as URI
import qualified Network.WebSockets         as WS
import qualified Network.WebSockets.Stream  as WS
import           Network.Wreq
import qualified OpenSSL                    as SSL
import qualified OpenSSL.Session            as SSL
import qualified System.IO.Streams.Internal as StreamsIO
import           System.IO.Streams.SSL (sslToStreams)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Web.Slack.Send (ping, sendMessage)
import           Web.Slack.Types


type SlackBot  = Event -> Slack ()
type URLString = String


ioUserError :: String -> LoggingT IO a
ioUserError s = do
  logErrorN $ T.pack s
  liftIO . ioError $ userError s


rtmStart :: SlackConfig -> LoggingT IO (URLString, SlackSession)
rtmStart conf = do
  let token = conf ^. slackApiToken
  r <- liftIO . get $ "https://slack.com/api/rtm.start?token=" ++ token
  case (r ^? responseBody) of
    Nothing -> ioUserError $ "No response body: " ++ show r
    Just resp -> do
      let Just (BoolPrim ok) = resp ^? key "ok" . _Primitive
      unless ok $
        ioUserError $ "Unable to connect: " ++ (resp ^. key "error" . _String . unpacked)
      let wsUrl = resp ^. key "url" . _String . unpacked
      case eitherDecode resp of
        Left e -> ioUserError e
        Right session -> do
          logInfoN "Connected"
          return (wsUrl, session)


-- | Setup an SSL connection. Must be run in SSL.withOpenSSL
setupSSL :: String -> IO SSL.SSL
setupSSL host = do
  ctx <- SSL.context
  addrInfo  <- S.getAddrInfo Nothing (Just host) (Just "443")
  let addr = S.addrAddress $ head addrInfo
      fam = S.addrFamily $ head addrInfo
  sock <- S.socket fam S.Stream S.defaultProtocol
  S.connect sock addr
  ssl <- SSL.connection ctx sock
  SSL.connect ssl
  return ssl


-- | Run a `SlackBot`. The supplied bot will respond to all events sent by
-- the Slack RTM API.
--
-- Be warned that this function will throw an `IOError` if the connection
-- to the Slack API fails.
runBot :: SlackConfig -> SlackBot -> LoggingT IO ()
runBot conf bot = do
  (wsUrl, session) <- rtmStart conf
  loggerIO <- askLoggerIO
  queue <- liftIO newTQueueIO
  case parseWebSocketUrl wsUrl of
    Nothing -> error $ "Couldn't parse WebSockets URL: " ++ wsUrl
    Just (host, path) ->
      liftIO . SSL.withOpenSSL $ do
        (sslIn, sslOut) <- sslToStreams =<< setupSSL host
        (stream :: WS.Stream) <-
          WS.makeStream (StreamsIO.read sslIn)
                        (\b -> StreamsIO.write (B.toStrict <$> b) sslOut)
        WS.runClientWithStream stream host path WS.defaultConnectionOptions []
          (\conn -> let botSession = SlackBotSession
                                     { _connection = conn
                                     , _slackSession = session
                                     , _logger = loggerIO
                                     , _sendQueue = queue
                                     , _config = conf
                                     }
                    in runBotSession botSession bot)
  where
    parseWebSocketUrl :: URLString -> Maybe (String, String)
    parseWebSocketUrl url = do
      uri  <- URI.parseURI url
      name <- URI.uriRegName <$> URI.uriAuthority uri
      return (name, URI.uriPath uri)


runBotSession :: SlackBotSession -> SlackBot -> IO ()
runBotSession session bot = do
  liftIO $ WS.forkPingThread (session ^. connection) 10
  void . liftIO . forkIO $ runSlackBot session sender
  void . liftIO . forkIO $ runSlackBot session pinger
  runSlackBot session $ receiver bot


pinger :: Slack ()
pinger = void . forever $ do
  liftIO $ threadDelay 1000000 -- 1 second
  ping


-- | Dedicated thread for sending pings and messages to Slack.
-- This thread tracks the required, unique IDs for each message.
sender :: Slack ()
sender = loop 1
 where
  loop :: Int -> Slack ()
  loop msgId = do
    session <- ask
    msg <- liftIO . atomically $ readTQueue (session ^. sendQueue)
    payload <- case msg of
      SendPing -> encode <$> do
        now <- round <$> (liftIO getPOSIXTime)
        return $ PingPayload msgId "ping" now
      SendMessage cid txt ->
        return . encode $ MessagePayload msgId "message" cid txt
    logDebugNS "sender" $ "Sending: " <> T.pack (BC.unpack payload)
    liftIO $ WS.sendTextData (session ^. connection) payload
    loop $ msgId + 1


receiver :: SlackBot -> Slack ()
receiver bot = void . forever $ do
  session <- ask
  raw <- liftIO . WS.receiveData $ session ^. connection
  let rawText = T.pack (BC.unpack raw)
  logDebugNS "receiver" $ "Received: " <> rawText
  case eitherDecode raw of
    Left decodeErr -> do
      err $ "Failed to decode message: " <> rawText
      err $ T.pack decodeErr
      err "Please report this failure to the github issue tracker"
    Right event@(UnknownEvent e) -> do
      err $ "Failed to parse to a known event: " <> T.pack (show e)
      err "Please report this failure to the github issue tracker"
      -- Still handle the event if a user wants to
      go session event
    Right event -> go session event
 where
  err = logErrorNS "receiver"
  go :: SlackBotSession -> Event -> Slack ()
  go session event = void . liftIO . forkIO . runSlackBot session $ bot event
