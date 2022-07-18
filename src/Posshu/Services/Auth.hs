module Posshu.Services.Auth (
  Query(..),
  User(..),
  LoginError(..),
  LoginResponse(..),
  LoginArgs(..),
) where

import Universum.Base      (Generic)
import Universum.String    (Text)
import Universum.Monad     (Maybe)

import Data.Morpheus.Types (GQLType)

data User = User
  { fullName :: Text
  , power    :: Maybe Text
  } deriving (Generic, GQLType)

data LoginError
  = CannotFindUsername
  deriving (Generic, GQLType)

data LoginResponse
  = LoginOk User
  | LoginError [LoginError]
  deriving (Generic, GQLType)

data LoginArgs = LoginArgs
  { login     :: Text
  , password  :: Text
  } deriving (Generic, GQLType)

data Query m = Query
  { deity :: LoginArgs -> m LoginResponse
  } deriving (Generic, GQLType)
