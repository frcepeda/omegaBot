{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.Wai.Handler.Warp
import Control.Concurrent
import Control.Applicative
import System.IO
import System.Environment
import qualified OmegaUp as OUp
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import qualified Slack as S
import qualified Data.ByteString.Char8 as C8

main = do
    [user, pass, path] <- words <$> readFile "config"

    (Just auth) <- OUp.login user pass
    --print =<< OUp.query "/api/session/currentsession" (Just auth) []

    forkIO $ runEnv 4353 (S.commandHandler auth)

    (output, input) <- spawn unbounded
    OUp.subscribe auth "wstest" output

    runEffect $ fromInput input >-> (toSlack (C8.pack path))

    where toSlack p = do
            w <- await
            lift $ C8.putStrLn $ OUp.toSlack w
            lift $ S.postMessage p (OUp.toSlack w)
            toSlack p
