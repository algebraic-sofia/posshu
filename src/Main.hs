module Main where

import Universum (IO, putTextLn, ($))

import Web.Scotty                  (scotty)
import Posshu.Environment (GithubData(..), Environment (..))
import qualified Polysemy
import Network.HTTP.Req (defaultHttpConfig)
import Posshu.Services.Auth.Github (githubRoutes)

main :: IO ()
main = do

  let gbData = GithubData "" ""
  let env    = Environment gbData defaultHttpConfig

  scotty 8080 $ do
    githubRoutes env