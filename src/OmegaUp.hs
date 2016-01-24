{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OmegaUp
    ( login
    , AuthToken
    , query
    , subscribe
    ) where

import Network.Connection (TLSSettings (..))
import qualified Network.WebSockets as WS
import qualified Wuss as WS
import Network.HTTP.Conduit
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Default
import Data.Function
import Data.Aeson
import Debug.Trace
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8

data AuthToken = UserToken C8.ByteString
               | PublicToken C8.ByteString
    deriving (Show)

data ClarificationData = ClarificationData
    { clarification_id :: Int
    , contest_alias :: String
    , problem_alias :: String
    , author :: Maybe String
    --, time :: DateTime
    , message :: String
    , answer :: Maybe String
    } deriving (Show)

instance Eq ClarificationData where
    (==) = (==) `on` clarification_id 

data ContestEvent = RunUpdate
                  | ClarificationUpdate ClarificationData
                  | ScoreboardUpdate
    deriving (Show, Eq)

query :: C8.ByteString -> Maybe AuthToken -> [(C8.ByteString, Maybe C8.ByteString)] -> IO (Response LB.ByteString)
query path auth opts = withManager (\man -> httpLbs req man)
    where req = setQueryString opts' q
          q = def
            { host = "omegaup.com"
            , port = 443
            , secure = True
            , path = path
            }
          opts' = case auth of
                    Nothing              -> opts
                    Just (UserToken t)   -> ("auth_token", Just t) : opts
                    Just (PublicToken t) -> ("token", Just t) : opts

login :: String -> String -> IO (Maybe AuthToken)
login user pass = do
    let opts = [("usernameOrEmail", Just (C8.pack user))
               ,("password", Just (C8.pack pass))]
    response <- query "/api/user/login" Nothing opts
    --guard $ statusIsSuccessful (responseStatus response)
    let obj = decode (responseBody response) :: Maybe (M.Map String String)
    return $ UserToken . C8.pack <$> (M.lookup "auth_token" =<< obj)

subscribe :: AuthToken -> String -> IO (Maybe [ContestEvent])
subscribe (UserToken ouat) contestAlias = do
    let header = [("Cookie", "ouat=" <> ouat), ("Sec-WebSocket-Protocol", "com.omegaup.events")]
        path = "/api/contest/events/" ++ contestAlias
    Just <$> WS.runSecureClientWith "omegaup.com" 443 path WS.defaultConnectionOptions header loop
    where loop conn = do
            WS.sendDataMessage conn (WS.Text "ping")
            msg <- WS.receiveDataMessage conn
            traceShow msg (return ())
            (RunUpdate:) <$> loop conn
