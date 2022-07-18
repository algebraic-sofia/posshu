module Posshu.Environment (
  Environment(..),
  GithubData(..),
  interpretReqApp
) where

import Control.Monad.Reader (ReaderT, MonadReader)
import Data.Text            (Text)
import Network.HTTP.Req     (HttpConfig, MonadHttp, runReq)
import Polysemy.Req         (HttpConfig, handleHttpException, Req(..))
import Universum            (Proxy, Monad, Applicative, Functor, MonadIO (..), ($), print, error, IO, asks, throwM, MonadThrow(..))
import Polysemy             (Member, Embed, InterpreterFor, interpret, embed)

import qualified Network.HTTP.Req as R
import qualified Universum.Monad as Monad

-- | Configuration for the integration with github's
-- oAuth2 service.
data GithubData = GithubData
  { clientSecret :: Text
  , clientId     :: Text
  }

-- | The entire program configuration.
data Environment = Environment
  { githubData :: GithubData
  -- | Configuration for the "req" package.
  , httpConfig :: HttpConfig
  }

-- | The App main Monad transformer that is basically
-- the RIO Pattern
newtype AppReader a = AppReader { runApp' :: ReaderT Environment IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Environment, MonadThrow)

-- | This part creates an instance of MonadHttp to AppReader so I can
-- use the AppReader instead of the Polysemy.Req

instance MonadHttp AppReader where
  handleHttpException e = throwM e
  getHttpConfig = asks httpConfig

runApp :: MonadIO m => Environment -> AppReader a -> m a
runApp env app = liftIO $ Monad.runReaderT (runApp' app) env

interpretReqApp :: Member (Embed IO) r => Environment -> InterpreterFor Req r
interpretReqApp env = interpret $ \case
  Req m u b p o -> embed @IO $ runApp env $ R.req m u b p o
