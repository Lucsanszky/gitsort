{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
( run
) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

type GitRepoAPI = "users"
               :> Header "User-Agent" Text
               :> Capture "username" Text 
               :> "repos" 
               :> Get '[JSON] [Repo]

data Repo = Repo 
  { name :: Text
  , size :: Int
  } deriving (Show, Generic)

instance ToJSON Repo
instance FromJSON Repo

api :: Proxy GitRepoAPI
api = Proxy

getRepos :: Maybe Text -> Text
                 -> Manager 
                 -> BaseUrl 
                 -> ExceptT ServantError IO [Repo]
getRepos = client api

run :: IO ()
run = do
	manager <- newManager tlsManagerSettings
	result  <- runExceptT $ getRepos (Just "Lucsanszky.github.io") "Lucsanszky" manager $ BaseUrl Https "api.github.com" 443 ""
	case result of
		Left err    -> putStrLn $ "Error: " ++ show err
		Right repos -> do
			print repos
