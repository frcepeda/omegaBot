{-# LANGUAGE OverloadedStrings #-}

module Slack
    ( postMessage
    ) where

import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Network.HTTP.Types.Method
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Monoid
import Data.Either
import Data.Default
import Data.Function
import Data.Aeson
import Debug.Trace
import qualified Data.ByteString.Char8 as C8

postMessage :: C8.ByteString -> C8.ByteString -> IO ()
postMessage path message = void $ withManager (\man -> httpLbs req man)
    where req = def
            { host = "hooks.slack.com"
            , port = 443
            , secure = True
            , path = path
            , method = methodPost
            , requestBody = RequestBodyBS message
            }
