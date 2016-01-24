{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.IO
import System.Environment
import qualified OmegaUp as OUp
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P

main = do
    [user, pass] <- getArgs

    (Just auth) <- OUp.login user pass
    print =<< OUp.query "/api/session/currentsession" (Just auth) []

    (output, input) <- spawn unbounded
    OUp.subscribe auth "wstest" output

    runEffect $ fromInput input >-> z

    where z = do
            w <- await
            lift $ print w
            z
