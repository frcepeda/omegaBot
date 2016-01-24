{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import System.Environment
import qualified OmegaUp as OUp

main = do
    [user, pass] <- getArgs
    (Just auth) <- OUp.login user pass
    print =<< OUp.query "/api/session/currentsession" (Just auth) []
    print =<< OUp.subscribe auth "wstest"
