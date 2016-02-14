{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Slack.Types where

import qualified Data.Text as T

data SlackCommand = Reply ClarificationResponse

data ClarificationResponse = ClarificationResponse
           { clarification_id :: T.Text
           , answer :: T.Text
           }
