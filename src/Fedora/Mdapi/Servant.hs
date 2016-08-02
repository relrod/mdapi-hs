{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Fedora.Mdapi.Servant where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson
import Data.Proxy
import qualified Data.Text as T
--import GHC.Generics
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Fedora.Mdapi.Types

type MdapiApi =
  "branches" :> Get '[JSON] [T.Text]
  :<|> Capture "branch" T.Text  :> "pkg" :> Capture "package" T.Text :> Get '[JSON] PackageResponse
  :<|> Capture "branch" T.Text  :> "files" :> Capture "package" T.Text :> Get '[JSON] FilesResponse
  :<|> Capture "branch" T.Text  :> "changelog" :> Capture "package" T.Text :> Get '[JSON] ChangelogResponse

api :: Proxy MdapiApi
api = Proxy

branches :: Manager -> BaseUrl -> ExceptT ServantError IO [T.Text]
pkg :: T.Text -> T.Text -> Manager -> BaseUrl -> ExceptT ServantError IO PackageResponse
files :: T.Text -> T.Text -> Manager -> BaseUrl -> ExceptT ServantError IO FilesResponse
changelog :: T.Text -> T.Text -> Manager -> BaseUrl -> ExceptT ServantError IO ChangelogResponse
branches :<|> pkg :<|> files :<|> changelog  = client api
