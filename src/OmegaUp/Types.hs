{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OmegaUp.Types where

import GHC.Generics
import Control.Monad
import Control.Applicative
import Data.Function
import Data.Aeson
import qualified Data.ByteString.Char8 as C8

data ClarificationData = ClarificationData
    { clarification_id :: String
    , contest_alias :: Maybe String
    , problem_alias :: String
    , author :: Maybe String
    --, time :: DateTime
    , message :: String
    , answer :: Maybe String
    } deriving (Show, Generic)

instance FromJSON ClarificationData

instance Eq ClarificationData where
    (==) = (==) `on` clarification_id 

data ContestEvent = RunUpdate
                  | ClarificationUpdate ClarificationData
                  | ScoreboardUpdate
    deriving (Show, Eq)

instance FromJSON ContestEvent where
    parseJSON = withObject "message" $ \m -> do
         ClarificationUpdate <$> m .: "clarification"
