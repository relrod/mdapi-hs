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

pkg :: Branch -> String -> MdapiT (Response BL.ByteString)
pkg branch package =
  mdapiGet $ "/" ++ branchToBranchName branch ++ "/pkg/" ++ package

files :: Branch -> String -> MdapiT (Response BL.ByteString)
files branch package =
  mdapiGet $ "/" ++ branchToBranchName branch ++ "/files/" ++ package

changelog :: Branch -> String -> MdapiT (Response BL.ByteString)
changelog branch package =
  mdapiGet $ "/" ++ branchToBranchName branch ++ "/changelog/" ++ package
