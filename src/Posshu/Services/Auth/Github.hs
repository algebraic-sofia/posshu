module Posshu.Services.Auth.Github (
  githubRoutes
) where

import Universum.Applicative ((<*>), Applicative (pure))
import Universum.Function    (($))
import Universum.Functor     ((<$>))
import Universum.String      (Text, ByteString, ConvertUtf8 (decodeUtf8))
import Universum.Monoid      ((<>), Monoid (mempty))
import Universum.Monad       (lift, MonadIO (liftIO), Maybe (Nothing))
import Universum             (print, Show, undefined, putText, IO, putTextLn, Either (..), catch, show, Maybe (..))

import Posshu.Environment    (GithubData(..), Environment)

import Data.ByteString.Char8 (putStrLn)
import Data.Aeson            (Value, ToJSON(..), object, (.=), FromJSON(parseJSON), fromJSON, (.:), withObject)
import Web.Scotty            (ScottyM, param, get, text)
import Network.HTTP.Req      ((/:), Scheme(Http), HttpException (..))

import Polysemy.Req          (Req)
import Polysemy.Error        (Error)
import Polysemy              (Members, Sem, Embed, run, Member, embed)

import Universum.Exception   (Exception, try)
import GHC.Exception         (throw)
import Polysemy.Reader       (Reader, ask)

import qualified Polysemy.Req       as Req
import qualified Polysemy.Error     as Error
import qualified Polysemy.IO        as IO
import qualified Polysemy.Reader    as Reader
import qualified Polysemy           as Sem
import qualified Posshu.Environment as Env

-- Data Structures

data AccessTokenRequest = AccessTokenRequest
  { clientId     :: Text
  , clientSecret :: Text
  , code         :: Text
  }

data AccessTokenResponse = AccessTokenResponse
  { tokenType   :: Text
  , scope       :: Text
  , accessToken :: Text
  } deriving Show

instance ToJSON AccessTokenRequest where
  toJSON request = object
    [ "client_id"     .= request.clientId
    , "client_secret" .= request.clientSecret
    , "code"          .= request.code
    ]

instance FromJSON AccessTokenResponse where
  parseJSON = withObject "AccessTokenResponse" $ \v -> AccessTokenResponse
    <$> v .: "token_type"
    <*> v .: "scope"
    <*> v .: "access_token"

-- Urls

githubUrl :: Req.Url 'Http
githubUrl = Req.http "www.github.com"

oauthUrl :: Req.Url 'Http
oauthUrl = githubUrl /: "login" /: "oauth" /: "access_token"

-- Request

requestToken :: Members '[Reader GithubData, Req] m => Text -> Sem m AccessTokenResponse
requestToken code = do
    gbData :: GithubData <- Reader.ask
    let request = AccessTokenRequest gbData.clientId gbData.clientSecret code
    response <- getAccessToken request
    pure (Req.responseBody response)
  where
    getAccessToken request = Req.req Req.POST oauthUrl (Req.ReqBodyJson request) Req.jsonResponse mempty

githubRoutes :: Environment -> ScottyM ()
githubRoutes env = do
  get "/auth/github" $ do
    code <- param "code"
    result <- handleHttpErr $ lift (Sem.runM (runReq (requestToken code)))
    case result of
      Left err  -> text "Oh no, something went wrong!:"
      Right err -> text "Logging In!!"
  where
    runReq action = Env.interpretReqApp env $ Reader.runReader env.githubData action
    handleHttpErr action = catch (Right <$> action) $ \case
      VanillaHttpException err -> pure $ Left "Cannot send request to Github!"
      JsonHttpException err    -> pure $ Left "Invalid JSON Body from Github"