-----------------------------------------------------------------------------
-- |
-- Module : Fedora.Mdapi
-- Copyright : (C) 2015 Red hat, Inc.
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Mdapi API Client
----------------------------------------------------------------------------
module Fedora.Mdapi where

import Control.Lens
import Data.Aeson
--import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Lazy.Char8 as BL
import Fedora.Mdapi.Internal
import Fedora.Mdapi.Types
import Network.Wreq

pkg :: Branch -> String -> MdapiT (Either String PackageResponse)
pkg branch package = do
  rawResp <- mdapiGet $ "/" ++ branchToBranchName branch ++ "/pkg/" ++ package
  return $ eitherDecode (rawResp ^. responseBody)

files :: Branch -> String -> MdapiT (Either String FilesResponse)
files branch package = do
  rawResp <- mdapiGet $ "/" ++ branchToBranchName branch ++ "/files/" ++ package
  return $ eitherDecode (rawResp ^. responseBody)

changelog :: Branch -> String -> MdapiT (Either String ChangelogResponse)
changelog branch package = do
  rawResp <- mdapiGet $
             "/" ++ branchToBranchName branch ++ "/changelog/" ++ package
  -- https://pagure.io/mdapi/issue/23
  --resp <- asJSON =<< rawResp
  return $ eitherDecode (rawResp ^. responseBody)
