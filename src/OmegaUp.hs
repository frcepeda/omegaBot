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
    response <- httpLbs req manager
    --traceShowM response
    return response
    where req = setQueryString opts' q
          q = defaultRequest { host = "omegaup.com"
                             , port = 443
                             , secure = True
                             , path = path
                             }
          opts' = case auth of
                    Nothing              -> opts
                    Just (UserToken t)   -> ("ouat", Just t) : opts
                    Just (PublicToken t) -> ("token", Just t) : opts

login :: String -> String -> IO (Maybe AuthToken)
login user pass = do
    let opts = [("usernameOrEmail", Just (C8.pack user))
               ,("password", Just (C8.pack pass))]
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
    let opts = [("contest_alias", Just $ T.encodeUtf8 alias)
               ]

    traceShowM opts

    r <- query "/api/contest/adminDetails" (Just auth) opts

    let obj = decode' (responseBody r)

    return $ maybe undefined id obj

subscribe :: AuthToken -> String -> Output ContestEvent -> IO ()
subscribe token@(UserToken ouat) contestAlias output = do
    details <- adminDetails token (T.pack contestAlias)

    traceShowM details

    let psetId = toInt $ problemset_id details

    let header = [("Cookie", "ouat=" <> ouat)
                 ,("Sec-WebSocket-Protocol", "com.omegaup.events")
                 ]
        path = "/events/?filter=/problemset/" ++ (show psetId)

    liftIO . void . forkIO $ WS.runSecureClientWith "omegaup.com"
                                                    443
                                                    path
                                                    WS.defaultConnectionOptions
                                                    header
                                                    manage

    where manage :: WS.Connection -> IO ()
          manage conn = WS.withPingThread conn 30 (putStrLn $ "ping: " ++ contestAlias) $ do

              runEffect $ loop >-> toOutput output

              where loop = do
                      WS.Text m _ <- lift $ WS.receiveDataMessage conn

                      traceShowM m

                      let eventP = eitherDecode' m

                      case eventP of
                          Right x -> yield $ (x :: ContestEvent)
                          Left l -> traceShowM l

                      loop
