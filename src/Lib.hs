{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
( runApp
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (sortBy)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Media ((//))
import Network.Wai 
import Network.Wai.Handler.Warp (run)
import qualified Servant as S (throwError)
import Servant.API 
import Servant.API.ContentTypes (eitherDecodeLenient)
import Servant.Client
import Servant.Server (Server, err404, errBody, serve)

-- | Data structure for the JSON data
data Repo = Repo 
  { name             :: Text
  , full_name        :: Text
  , size             :: Int
  , description      :: Maybe Text
  , language         :: Maybe Text
  , created_at       :: Text
  , clone_url        :: Text
  , stargazers_count :: Int
  } deriving (Show, Generic)

data PrettyJSON

instance ToJSON Repo

instance FromJSON Repo 

instance Accept PrettyJSON where
    contentType _ = "application" // "json"

instance ToJSON a => MimeRender PrettyJSON a where
    mimeRender _ = encodePretty

instance FromJSON a => MimeUnrender PrettyJSON a where
    mimeUnrender _ = eitherDecodeLenient

-- | Type declaration for describing a GitHub API endpoint
-- reachable at: /users/:username/repos
type GitRepoAPI = "users"
               :> Header "User-Agent" Text
               :> Capture "username" Text 
               :> "repos" 
               :> Get '[PrettyJSON] [Repo]

-- | Type declaration for describing an endpoint
-- reachable at: /:username
type MyAPI = Capture "username" Text 
          :> Get '[PrettyJSON] [Repo]

gitAPI :: Proxy GitRepoAPI
gitAPI = Proxy

myAPI :: Proxy MyAPI
myAPI = Proxy

-- | Function for querying the GitHub API
getRepos :: Maybe Text -> Text
                         -> Manager 
                         -> BaseUrl 
                         -> ExceptT ServantError IO [Repo]
getRepos = client gitAPI

sortRepos :: [Repo] -> [Repo]
sortRepos = sortBy repoSize

repoSize :: Repo -> Repo -> Ordering
repoSize (Repo {size = s1}) (Repo {size = s2})
  | s2 < s1   = LT
  | otherwise = GT

-- | Given a user handle, it attempts to retrieve
-- the five largest GitHub repositories of the given user
-- and list them in descending order
server :: Server MyAPI
server user = do
  manager <- liftIO $ newManager tlsManagerSettings
  result  <- liftIO $ runExceptT 
           $ getRepos (Just "lucsanszky") user manager 
           $ BaseUrl Https "api.github.com" 443 ""

  case result of
    Right repos -> return $ (take 5) $ sortRepos repos
    Left err    -> S.throwError 
                 $ err404 { errBody = pack $ "GitHub Error: " ++ show err}

app :: Application
app = serve myAPI server

runApp :: IO ()
runApp = run 3333 app
