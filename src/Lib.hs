{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
( runApp
) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import Data.Text
import qualified Data.Text.IO as T (getLine)
import GHC.Generics
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant.API
import Servant.Client
import Servant.Server

type GitRepoAPI = "users"
               :> Header "User-Agent" Text
               :> Capture "username" Text 
               :> "repos" 
               :> Get '[JSON] [Repo]

type MyAPI = Capture "username" Text 
          :> Get '[JSON] [Repo]

data Repo = Repo 
  { name :: Text
  , size :: Int
  } | NotFound 
      { documentation_url :: Text
      , message :: Text
      } deriving (Show, Generic)

instance ToJSON Repo
instance FromJSON Repo

gitAPI :: Proxy GitRepoAPI
gitAPI = Proxy

myAPI :: Proxy MyAPI
myAPI = Proxy

getRepos :: Maybe Text -> Text
                       -> Manager 
                       -> BaseUrl 
                       -> ExceptT ServantError IO [Repo]
getRepos = client gitAPI

server :: Server MyAPI
server = pingGitHub 
  where pingGitHub :: Text -> Handler [Repo]
        pingGitHub user = liftIO $ do
          manager <- newManager tlsManagerSettings
          result  <- runExceptT 
                   $ getRepos (Just "lucsanszky.github.io") user manager 
                   $ BaseUrl Https "api.github.com" 443 ""

          case result of
          	Right repos -> return repos
          	Left err    -> return 
          	             $ [NotFound "https://developer.github.com/v3" "User Not Found"]

app :: Application
app = serve myAPI server

runApp :: IO ()
runApp = run 3333 app
