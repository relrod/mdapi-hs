-----------------------------------------------------------------------------
-- |
-- Module : Fedora.Mdapi.Types
-- Copyright : (C) 2015 Red hat, Inc.
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Types for Mdapi
----------------------------------------------------------------------------
module Fedora.Mdapi.Types where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson
--import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import Data.List (dropWhileEnd)
import Data.Monoid
import Network.Wreq
import Network.Wreq.Types (Postable)

-- | Our 'MdapiT' type which is really a 'ReaderT' with 'IO' as its base. For
-- now, at least.
type MdapiT a = ReaderT MdapiConfig IO a

-- | Run the whole transformer stack.
runMdapiT :: MdapiT a -> MdapiConfig -> IO a
runMdapiT = runReaderT

-- | Describes how to connect to the mdapi instance.
data MdapiConfig = MdapiConfig {
    _baseUrl :: String
  } deriving (Eq, Show)

-- | Default to <https://apps.fedoraproject.org/mdapi/>.
instance Default MdapiConfig where
  def = MdapiConfig "https://apps.fedoraproject.org/mdapi/"

data Branch =
    Dist6Epel
  | F23
  | Rawhide
  | F22
  | Koji
  | Dist5Epel
  | Dist6
  | Dist5
  | Epel7
  | F21

branchToBranchName :: Branch -> String
branchToBranchName Dist6Epel = "dist-6E-epel"
branchToBranchName F23       = "f23"
branchToBranchName Rawhide   = "rawhide"
branchToBranchName F22       = "f22"
branchToBranchName Koji      = "koji"
branchToBranchName Dist5Epel = "dist-5E-epel"
branchToBranchName Dist6     = "dist-6E"
branchToBranchName Dist5     = "dist-5E"
branchToBranchName Epel7     = "epel7"
branchToBranchName F21       = "f21"
