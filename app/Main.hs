{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified OmegaUp as OUp
import qualified OmegaUp.Types as OUp
import qualified Slack as S

import Network.Wai.Handler.Warp
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import System.IO
import System.Environment
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified Data.ByteString.Char8 as C8
import Data.Maybe

data AuthInfo = UsernamePassword C8.ByteString C8.ByteString
              | ApiToken C8.ByteString
    deriving (Read, Show)

data BotConfig = BotConfig
    { authorization :: AuthInfo
    , tempAuthorization :: AuthInfo -- quark doesn't support api tokens :c
    , path :: C8.ByteString
    , contests :: [String]
    } deriving (Read, Show)

main = do
    config <- read <$> readFile "config"

    let configToAuth authInfo =
            case authInfo of
                UsernamePassword user pass -> fromJust <$> OUp.login user pass
                ApiToken token -> return . OUp.ApiToken $ token
    
    auth <- configToAuth $ authorization config
    print =<< OUp.query "/api/session/currentsession" (Just auth) []

    broadcasterAuth <- configToAuth $ tempAuthorization config
    print =<< OUp.query "/api/session/currentsession" (Just broadcasterAuth) []

    mcontests <- newMVar (contests config)

    let cfg = S.HandlerConfig
                { S.slackUrl = path config
                , S.subscriptions = mcontests
                , S.auth = auth
                }

    forkIO $ runEnv 59947 (S.commandHandler cfg)

    (output, input) <- spawn unbounded

    forM_ (contests config) $ \c -> OUp.subscribe broadcasterAuth c output

    runEffect $ fromInput input >-> forever (toSlack (path config))

    where toSlack p = lift . S.postMessage p =<< await
