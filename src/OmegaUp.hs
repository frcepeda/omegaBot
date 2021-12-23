{-# LANGUAGE OverloadedStrings #-}

module OmegaUp
    ( module OmegaUp.Types
    , login
    , query
    , answerClarification
    , clarifications
    , unansweredClarifications
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
import Data.Maybe
import Data.Monoid
import Data.Either
import Data.Default
import Data.Function
import Data.Aeson
import Data.Aeson.Types
import Debug.Trace
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import System.IO

query :: C8.ByteString -> Maybe AuthToken -> [(C8.ByteString, Maybe C8.ByteString)] -> IO (Response LB.ByteString)
query path auth opts = do
    manager <- newManager tlsManagerSettings
    httpLbs req manager
    where req = setQueryString (authOpts ++ opts) q
          q = defaultRequest { host = "omegaup.com"
                             , port = 443
                             , secure = True
                             , path = path
                             , requestHeaders = headers
                             }
          authOpts = case auth of
                        Just (UserToken t)   -> [("ouat", Just t)]
                        Just (PublicToken t) -> [("token", Just t)]
                        _                    -> []
          headers = case auth of
                        Just (ApiToken t) -> [("Authorization", "token " <> t)]
                        _                 -> []

login :: C8.ByteString -> C8.ByteString -> IO (Maybe AuthToken)
login user pass = do
    let opts = [("usernameOrEmail", Just user)
               ,("password", Just pass)]
    response <- query "/api/user/login" Nothing opts
    --guard $ statusIsSuccessful (responseStatus response)
    let obj = decode (responseBody response) :: Maybe (M.Map String String)
    return $ UserToken . C8.pack <$> (M.lookup "auth_token" =<< obj)

answerClarification :: AuthToken -> T.Text -> T.Text -> Bool -> IO (Response LB.ByteString)
answerClarification auth cId answer public = do
    let opts = [("clarification_id", Just $ T.encodeUtf8 cId)
               ,("answer", Just $ T.encodeUtf8 answer)
               ,("public", Just $ if public then "1" else "0")
               ]

    query "/api/clarification/update" (Just auth) opts

clarifications :: AuthToken -> T.Text -> IO [ClarificationData]
clarifications auth cId = do
    let opts = [("contest_alias", Just $ T.encodeUtf8 cId)
               ]

    r <- query "/api/contest/clarifications" (Just auth) opts

    let obj = decode' (responseBody r)
    let cs = obj >>= M.lookup ("clarifications" :: String)
                 >>= parseMaybe parseJSON

    return $ maybe [] id cs

unansweredClarifications :: AuthToken -> T.Text -> IO [ClarificationData]
unansweredClarifications auth cId = filter unanswered <$> clarifications auth cId
    where unanswered = isNothing . answer

adminDetails :: AuthToken -> T.Text -> IO AdminDetails
adminDetails auth alias = do
    let opts = [("contest_alias", Just $ T.encodeUtf8 alias)]

    traceShowM opts

    r <- query "/api/contest/adminDetails" (Just auth) opts

    return . fromJust . decode' . responseBody $ r

subscribe :: AuthToken -> String -> Output ContestEvent -> IO ()
subscribe auth contestAlias output = do
    details <- adminDetails auth (T.pack contestAlias)

    traceShowM details

    let psetId = toInt $ problemset_id details

    let header = [("Cookie", "ouat=" <> ouat) | UserToken ouat <- [auth]] ++
                 [("Authorization", "token " <> token) | ApiToken token <- [auth]] ++
                 [("Sec-WebSocket-Protocol", "com.omegaup.events")]
        path = "/events/?filter=/problemset/" ++ show psetId
    
    traceShowM header

    liftIO . void . forkIO $ WS.runSecureClientWith "omegaup.com"
                                                    443
                                                    path
                                                    WS.defaultConnectionOptions
                                                    header
                                                    manage

    where manage :: WS.Connection -> IO ()
          manage conn = WS.withPingThread conn 30 (putStrLn $ "ping: " ++ contestAlias) $ do

              traceM "connected!"

              runEffect $ loop >-> toOutput output

              where loop = forever $ do
                      WS.Text m _ <- lift $ WS.receiveDataMessage conn

                      let eventP = eitherDecode' m
                      
                      traceShowM eventP

                      case eventP of
                          Right x -> yield (x :: ContestEvent)
                          Left l -> return ()
