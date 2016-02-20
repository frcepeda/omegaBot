{-# LANGUAGE OverloadedStrings #-}

module Slack
    ( postMessage
    , commandHandler
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

postMessage :: C8.ByteString -> C8.ByteString -> IO ()
postMessage path message = void $ C.withManager (\man -> C.httpLbs req man)
    where req = def
            { C.host = "hooks.slack.com"
            , C.port = 443
            , C.secure = True
            , C.path = path
            , C.method = methodPost
            , C.requestBody = C.RequestBodyBS message
            }

commandHandler :: AuthToken -> Application
commandHandler auth req respond = do
    let args = queryString req
    case jlookup "command" args of
        "/answer" -> do
            let text = T.decodeUtf8 $ jlookup "text" args
            let (cId, rest) = T.span (/= ' ') text
            r <- answerClarification auth cId (T.tail rest) False
            reply (C.responseBody r)
        "/answer-public" -> do
            let text = T.decodeUtf8 $ jlookup "text" args
            let (cId, rest) = T.span (/= ' ') text
            r <- answerClarification auth cId (T.tail rest) True
            reply (C.responseBody r)
        _ -> reply "Unknown command."
    where reply = respond . responseLBS status200 []
          jlookup k l = fromJust (fromJust (lookup k l))
