{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------
-- |
-- Example:
-- > import Slack.Bot
-- > import Control.Monad.Logger (runStdoutLoggingT)
-- >
-- > -- type SlackBot = Event -> Slack ()
-- > echoBot :: SlackBot
-- > echoBot (Message cid _ msg _ _ _) = sendMessage cid msg
-- > echoBot _ = return ()
-- >
-- > main :: IO ()
-- > main = runStdoutLoggingT $ runBot "{- slack token goes here -}" echoBot
--
module Slack.Bot
( runBot
, Slack(..)
, SlackBot
, Token
, BotSession(..)
, apiToken
, slackSession
, connection
, sendQueue
, logger
, ping
, sendMessage

-- Re-exports
, liftIO
, module Slack.API
) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TQueue ( TQueue
                                               , newTQueueIO
                                               , readTQueue
                                               , writeTQueue
                                               )
import           Control.Lens
import           Control.Monad (forever, void, unless)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Monad.Reader ( MonadReader
                                      , ReaderT
                                      , runReaderT
                                      , ask
                                      , asks
                                      )
import           Control.Monad.Logger ( LoggingT
                                      , MonadLogger
                                      , runLoggingT
                                      , logDebugNS
                                      , logInfoN
                                      , askLoggerIO
                                      , logErrorN
                                      , logErrorNS
                                      , Loc
                                      , LogStr
                                      , LogLevel
                                      , LogSource
                                      )
import           Data.Aeson (encode, eitherDecode)
import           Data.Aeson.Lens (key, _String, _Primitive, Primitive(BoolPrim))
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Monoid ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Lens (unpacked)
import qualified Network.Socket             as S
import qualified Network.URI                as URI
import qualified Network.WebSockets         as WS
import qualified Network.WebSockets.Stream  as WS
import           Network.Wreq (get, responseBody)
import qualified OpenSSL                    as SSL
import qualified OpenSSL.Session            as SSL
import qualified System.IO.Streams.Internal as StreamsIO
import           System.IO.Streams.SSL (sslToStreams)
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Slack.API


type SlackBot  = Event -> Slack ()

type URLString = String
type Token = String


newtype Slack a = Slack {unSlack :: ReaderT BotSession (LoggingT IO) a}
  deriving ( Monad, Functor, Applicative, MonadIO
           , MonadReader BotSession
           , MonadLogger
           )

data SendPayload = SendPing
                 | SendMessage ChannelId Text

type SlackSendQueue = TQueue SendPayload

data BotSession = BotSession
  { _sendQueue    :: SlackSendQueue
  , _apiToken     :: Token        -- ^ Slack API Token
  , _slackSession :: SlackSession -- ^ Session information from the
                                  -- start of the connection
  , _connection   :: WS.Connection
  , _logger       :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
  }

makeLenses ''BotSession

runSlackBot :: BotSession -> Slack () -> IO ()
runSlackBot session = (`runLoggingT` (session ^. logger))
                    . (`runReaderT` session)
                    . unSlack


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


ioUserError :: String -> LoggingT IO a
ioUserError s = do
  logErrorN $ T.pack s
  liftIO . ioError $ userError s


rtmStart :: Token -> LoggingT IO (URLString, SlackSession)
rtmStart token = do
  r <- liftIO . get $ "https://slack.com/api/rtm.connect?token=" ++ token
  case (r ^? responseBody) of
    Nothing -> ioUserError $ "No response body: " ++ show r
    Just resp -> do
      let Just (BoolPrim ok) = resp ^? key "ok" . _Primitive
      unless ok $
        ioUserError $ "Unable to connect: "
                      ++ (resp ^. key "error" . _String . unpacked)
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
runBot :: Token -> SlackBot -> LoggingT IO ()
runBot token bot = do
  (wsUrl, session) <- rtmStart token
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
          (\conn -> let botSession = BotSession
                                     { _connection = conn
                                     , _slackSession = session
                                     , _logger = loggerIO
                                     , _sendQueue = queue
                                     , _apiToken = token
                                     }
                    in runBotSession botSession bot)
  where
    parseWebSocketUrl :: URLString -> Maybe (String, String)
    parseWebSocketUrl url = do
      uri  <- URI.parseURI url
      name <- URI.uriRegName <$> URI.uriAuthority uri
      return (name, URI.uriPath uri)


runBotSession :: BotSession -> SlackBot -> IO ()
runBotSession session bot = do
  liftIO $ WS.forkPingThread (session ^. connection) 10
  void . liftIO . forkIO $ runSlackBot session sender
  void . liftIO . forkIO $ runSlackBot session pinger
  runSlackBot session $ receiver bot


pinger :: Slack ()
pinger = void . forever $ do
  liftIO $ threadDelay 10000000 -- 10 seconds
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
  go :: BotSession -> Event -> Slack ()
  go session event = void . liftIO . forkIO . runSlackBot session $ bot event
