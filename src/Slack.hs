{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slack
    ( postMessage
    , commandHandler
    , HandlerConfig(..)
    ) where

import OmegaUp
import OmegaUp.Types
import Network.Wai
import Network.Connection (TLSSettings (..))
import qualified Network.HTTP.Conduit as C
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar 
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Either
import Data.Default
import Data.Function
import Data.Aeson
import Debug.Trace
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

postMessage :: Slackable a => C8.ByteString -> a -> IO ()
postMessage path message = void $ C.withManager (\man -> C.httpLbs req man)
    where req = def
            { C.host = "hooks.slack.com"
            , C.port = 443
            , C.secure = True
            , C.path = path
            , C.method = methodPost
            , C.requestBody = C.RequestBodyBS (toSlack message)
            }

data HandlerConfig = HandlerConfig
        { auth :: AuthToken
        , slackUrl :: C8.ByteString
        , subscriptions :: MVar [String]
        }

commandHandler :: HandlerConfig -> Application
commandHandler HandlerConfig{..} req respond = do
    let args = queryString req
    let text = T.decodeUtf8 $ jlookup "text" args
    let user = T.decodeUtf8 $ jlookup "user_name" args
    case jlookup "command" args of
        "/answer" -> do
            let (cId, rest) = T.span (/= ' ') text
            r <- answerClarification auth cId (T.tail rest) False
            postMessage slackUrl (user <> " replied to #" <> cId)
            reply (C.responseBody r)
        "/answer-public" -> do
            let (cId, rest) = T.span (/= ' ') text
            r <- answerClarification auth cId (T.tail rest) True
            postMessage slackUrl (user <> " replied to #" <> cId)
            reply (C.responseBody r)
        "/unanswered" -> do
            forkIO $ do
                contests <- readMVar subscriptions

                let getClars = unansweredClarifications auth . T.pack
                cs <- concat <$> forM contests getClars

                forM_ cs (postMessage slackUrl)

                when (null cs) $
                    postMessage slackUrl ("No unanswered clarifications." :: T.Text)
            reply "Looking for unanswered clarifications..."
        _ -> reply "Unknown command."
    where reply = respond . responseLBS status200 []
          jlookup k l = fromJust (fromJust (lookup k l))
