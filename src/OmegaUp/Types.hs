{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OmegaUp.Types where

import GHC.Generics
import Control.Monad
import Control.Applicative
import Data.Function
import Data.Maybe
import Data.Monoid
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as C8 (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Slack.Types

data AuthToken = UserToken C8.ByteString
               | PublicToken C8.ByteString
    deriving (Show)

newtype ClarificationID = ClarificationID { toText :: T.Text }
    deriving (Show, Eq)

instance A.FromJSON ClarificationID where
    parseJSON v
        = ClarificationID
        <$> (A.parseJSON v
             <|> ((T.pack . (show :: Integer -> String))
                  <$> A.parseJSON v))

data ClarificationData = ClarificationData
    { clarification_id :: ClarificationID
    , contest_alias :: Maybe T.Text
    , problem_alias :: T.Text
    , author :: T.Text
    --, time :: DateTime
    , message :: T.Text
    , answer :: Maybe T.Text
    } deriving (Show, Generic)

instance A.FromJSON ClarificationData

instance Eq ClarificationData where
    (==) = (==) `on` clarification_id 

instance Slackable ClarificationData where
    toSlack c = C8.toStrict . A.encode . obj $ [
            ("attachments"
            , arr [
                obj [("fallback", A.String fallback)
                    ,("pretext", A.String _title)
                    ,("color", A.String _color)
                    ,("fields"
                     , arr [
                         obj $ [("title", A.String _question)
                               ,("short", A.Bool False)
                               ] ++
                               (if _answered then
                                [("value", A.String _answer)]
                                else [])
                       ])
                    ]
              ])
           ]
           where obj = A.Object . HM.fromList
                 arr = A.Array . V.fromList
                 fallback = _title
                         <> "\nQ: \"" <> _question <> "\""
                         <> (if _answered then
                               "\nA: \"" <> _answer <> "\""
                             else "")
                 _title = "Clarification (#"
                         <> (toText . clarification_id) c
			 <> ") for " <> _problem <> " by " <> _author <> ":"
                 _problem = problem_alias c
                 _author = author c
                 _question = message c
                 _answered = maybe False (const True) (answer c)
                 _answer = fromJust $ answer c
                 _color = if _answered then green else red
                 green = "#52F71B"
                 red = "#F7521B"

instance Slackable ContestEvent where
    toSlack (ClarificationUpdate c) = toSlack c
    toSlack _ = "{\"text\":\"error\"}"

data ContestEvent = RunUpdate
                  | ClarificationUpdate ClarificationData
                  | ScoreboardUpdate
    deriving (Show, Eq)

instance A.FromJSON ContestEvent where
    parseJSON = A.withObject "message" $ \m -> do
         ClarificationUpdate <$> m A..: "clarification"
