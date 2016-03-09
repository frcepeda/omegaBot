{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Slack.Types
    ( Slackable(..)
    ) where

import Data.Monoid
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

class Slackable a where
    toSlack :: a -> C8.ByteString

instance Slackable T.Text where
    toSlack m = "{\"text\": \"" <> T.encodeUtf8 escaped <> "\"}"
        where escaped = T.replace "\"" "\\\"" m

