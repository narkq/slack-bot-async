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
, module Web.Slack.Types
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue ( readTQueue
                                               , newTQueueIO
                                               )
import           Control.Lens
import           Control.Monad (forever, void, unless)
import           Control.Monad.Reader       (runReaderT)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Text.Lens (unpacked)
import qualified Network.Socket             as S
import qualified Network.URI                as URI
import qualified Network.WebSockets         as WS
import qualified Network.WebSockets.Stream  as WS
import           Network.Wreq
import qualified OpenSSL                    as SSL
import qualified OpenSSL.Session            as SSL
import qualified System.IO.Streams.Internal as StreamsIO
import           System.IO.Streams.SSL      (sslToStreams)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Web.Slack.Types


type SlackBot = Event -> Slack ()

type URLString = String

ioUserError :: String -> IO a
ioUserError = ioError . userError

rtmStart :: SlackConfig -> IO (URLString, SlackSession)
rtmStart conf = do
  let token = conf ^. slackApiToken
  r <- get $ "https://slack.com/api/rtm.start?token=" ++ token
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
          putStrLn "rtm.start call successful"
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
runBot :: SlackConfig -> SlackBot -> IO ()
runBot conf bot = do
  (wsUrl, session) <- rtmStart conf
  case parseWebSocketUrl wsUrl of
    Nothing -> error $ "Couldn't parse WebSockets URL: " ++ wsUrl
    Just (host, path) ->
      SSL.withOpenSSL $ do
        (sslIn, sslOut) <- sslToStreams =<< setupSSL host
        (stream :: WS.Stream) <-
          WS.makeStream (StreamsIO.read sslIn)
                        (\b -> StreamsIO.write (B.toStrict <$> b) sslOut)
        WS.runClientWithStream stream host path WS.defaultConnectionOptions []
          (runBotSession conf session bot)
  where
    parseWebSocketUrl :: URLString -> Maybe (String, String)
    parseWebSocketUrl url = do
      uri  <- URI.parseURI url
      name <- URI.uriRegName <$> URI.uriAuthority uri
      return (name, URI.uriPath uri)

runBotSession :: SlackConfig -> SlackSession -> SlackBot -> WS.ClientApp ()
runBotSession conf session bot conn = do
  WS.forkPingThread conn 10
  queue <- newTQueueIO
  void . forkIO $ sender conn queue
  let slackBotSession =
        SlackBotSession { _sendQueue = queue
                        , _slackSession = session
                        , _config = conf
                        , _connection = conn
                        }
  botLoop slackBotSession bot

sender :: WS.Connection -> SlackSendQueue -> IO ()
sender conn queue = loop 1
 where
  loop msgId = do
    msg <- atomically $ readTQueue queue
    payload <- case msg of
      SendPing -> encode <$> do
        now <- round <$> getPOSIXTime
        return $ PingPayload msgId "ping" now
      SendMessage cid txt ->
        return . encode $ MessagePayload msgId "message" cid txt
    WS.sendTextData conn payload
    loop $ msgId + 1

botLoop :: SlackBotSession -> SlackBot -> IO ()
botLoop slackBotSession bot = void . forever $ do
  raw <- WS.receiveData $ slackBotSession ^. connection
  case eitherDecode raw of
    Left err -> do
      BC.putStrLn raw
      putStrLn err
      putStrLn "Please report this failure to the github issue tracker"
    Right event@(UnknownEvent e) -> do
      print e
      putStrLn "Failed to parse to a known event"
      putStrLn "Please report this failure to the github issue tracker"
      -- Still handle the event if a user wants to
      go event
    Right event -> go event
 where
  go :: Event -> IO ()
  go event = void . forkIO $ runReaderT (runSlack $ bot event) slackBotSession
