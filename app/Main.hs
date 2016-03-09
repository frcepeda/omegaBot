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

main = do
    [user, pass, path'] <- words <$> readFile "config"
    let path = C8.pack path'

    (Just auth) <- OUp.login user pass
    print =<< OUp.query "/api/session/currentsession" (Just auth) []

    let contests = ["OMIS2016NACIONAL"
                   ,"OMIP2016NACIONAL"
                   ,"OMI2016DIA1"
                   ,"OMI2016DIA1PUBLICO"
                   ,"OMIP2016NACIONALPUBLICO"
                   ,"OMIS2016NACIONALPUBLICO"
                   ]

    mcontests <- newMVar contests

    let cfg = S.HandlerConfig
                { S.slackUrl = path
                , S.subscriptions = mcontests
                , S.auth = auth
                }

    forkIO $ runEnv 4353 (S.commandHandler cfg)

    (output, input) <- spawn unbounded

    forM_ contests $ \c -> OUp.subscribe auth c output

    runEffect $ fromInput input >-> (forever $ toSlack path)

    where toSlack p = lift . S.postMessage p =<< await
