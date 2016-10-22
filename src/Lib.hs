{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
( runApp
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List (sortBy)
import Data.Proxy
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Servant as S (throwError)
import Servant.API
import Servant.Client
import Servant.Server

-- | Data structure for the JSON data
data Repo = Repo 
  { name             :: T.Text
  , full_name        :: T.Text
  , size             :: Int
  , description      :: Maybe T.Text
  , language         :: Maybe T.Text
  , created_at       :: T.Text
  , clone_url        :: T.Text
  , stargazers_count :: Int
  } deriving (Show, Generic)

instance ToJSON Repo
instance FromJSON Repo

-- | Type declaration for describing a GitHub API endpoint
-- reachable at: /users/:username/repos
type GitRepoAPI = "users"
               :> Header "User-Agent" T.Text
               :> Capture "username" T.Text 
               :> "repos" 
               :> Get '[JSON] [Repo]

-- | Type declaration for describing an endpoint
-- reachable at: /:username
type MyAPI = Capture "username" T.Text 
          :> Get '[JSON] [Repo]

gitAPI :: Proxy GitRepoAPI
gitAPI = Proxy

myAPI :: Proxy MyAPI
myAPI = Proxy

-- | Function for querying the GitHub API
getRepos :: Maybe T.Text -> T.Text
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
                 $ err404 { errBody = B.pack $ "GitHub Error: " ++ show err}

app :: Application
app = serve myAPI server

runApp :: IO ()
runApp = run 3333 app
