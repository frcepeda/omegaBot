{-# LANGUAGE OverloadedStrings #-}

module OmegaUp
    ( module OmegaUp.Types
    , login
    , query
    , subscribe
    ) where

import OmegaUp.Types

import Network.Connection (TLSSettings (..))
import qualified Network.WebSockets as WS
import qualified Wuss as WS
import Network.HTTP.Conduit
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad
import Data.Monoid
import Data.Either
import Data.Default
import Data.Function
import Data.Aeson
import Debug.Trace
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import System.IO

data AuthToken = UserToken C8.ByteString
               | PublicToken C8.ByteString
    deriving (Show)

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

subscribe :: AuthToken -> String -> Output ContestEvent -> IO ()
subscribe (UserToken ouat) contestAlias output = do
    let header = [("Cookie", "ouat=" <> ouat)
                 ,("Sec-WebSocket-Protocol", "com.omegaup.events")
                 ]
        path = "/api/contest/events/" ++ contestAlias

    void . liftIO $ WS.runSecureClientWith "omegaup.com"
                                           443
                                           path
                                           WS.defaultConnectionOptions
                                           header
                                           manage

    where manage :: WS.Connection -> IO ()
          manage conn = do
            void . forkIO . forever $ do -- FIXME: handle closed connection
                threadDelay (3*10^7 :: Int)
                WS.sendDataMessage conn (WS.Text "ping")

            void . forkIO . runEffect $ loop >-> toOutput output

            where loop = do
                    (WS.Text m) <- lift $ WS.receiveDataMessage conn

                    traceShow m (return ())

                    let eventP = eitherDecode' m
                        event = either (flip trace undefined) id eventP

                    yield $ (event :: ContestEvent)
                    loop
