{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified OmegaUp as OUp
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

data BotConfig = BotConfig
    { user :: String
    , pass :: String
    , path :: C8.ByteString
    , contests :: [String]
    } deriving (Read, Show)

main = do
    config <- read <$> readFile "config"

    (Just auth) <- OUp.login (user config) (pass config)
    print =<< OUp.query "/api/session/currentsession" (Just auth) []

    mcontests <- newMVar (contests config)

    let cfg = S.HandlerConfig
                { S.slackUrl = path config
                , S.subscriptions = mcontests
                , S.auth = auth
                }

    forkIO $ runEnv 4353 (S.commandHandler cfg)

    (output, input) <- spawn unbounded

    forM_ (contests config) $ \c -> OUp.subscribe auth c output

    runEffect $ fromInput input >-> (forever $ toSlack (path config))

    where toSlack p = lift . S.postMessage p =<< await
