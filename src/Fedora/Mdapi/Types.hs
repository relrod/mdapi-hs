{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T

data Branch =
    Dist6Epel
  | F24
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
branchToBranchName F24       = "f24"
branchToBranchName F23       = "f23"
branchToBranchName Rawhide   = "rawhide"
branchToBranchName F22       = "f22"
branchToBranchName Koji      = "koji"
branchToBranchName Dist5Epel = "dist-5E-epel"
branchToBranchName Dist6     = "dist-6E"
branchToBranchName Dist5     = "dist-5E"
branchToBranchName Epel7     = "epel7"
branchToBranchName F21       = "f21"

--------------------------------------------------------------------------------
-- Package
--------------------------------------------------------------------------------

data NEVR =
  NEVR { _name    :: Maybe T.Text
       , _epoch   :: Maybe T.Text
       , _version :: Maybe T.Text
       , _release :: Maybe T.Text
       } deriving (Eq, Show)

makeLenses ''NEVR

instance FromJSON NEVR where
  parseJSON (Object v) = NEVR <$>
                         v .: "name" <*>
                         v .: "epoch" <*>
                         v .: "version" <*>
                         v .: "release"
  parseJSON _          = mempty

data PackageResponse =
  PackageResponse { _arch :: T.Text
                  , _basename :: T.Text
                  , _copackages :: [T.Text]
                  , _conflicts :: [NEVR]
                  , _description :: T.Text
                  , _enhances :: [NEVR] -- Sometimes null, sometimes []?
                  , _pkgEpoch :: T.Text
                  , _obsoletes :: [NEVR]
                  , _provides :: [NEVR]
                  , _recommends :: [NEVR]
                  , _pkgRelease :: T.Text
                  , _pkgRepo :: T.Text
                  , _requires :: [NEVR]
                  , _suggests :: [NEVR]
                  , _summary :: T.Text
                  , _supplements :: [NEVR]
                  , _pkgVersion :: T.Text
                  } deriving (Eq, Show)

makeLenses ''PackageResponse

instance FromJSON PackageResponse where
  parseJSON (Object v) = PackageResponse <$>
                         v .: "arch" <*>
                         v .: "basename" <*>
                         v .: "co-packages" <*>
                         v .: "conflicts" <*>
                         v .: "description" <*>
                         fmap fixNull (v .: "enhances") <*>
                         v .: "epoch" <*>
                         v .: "obsoletes" <*>
                         v .: "provides" <*>
                         fmap fixNull (v .: "recommends") <*>
                         v .: "release" <*>
                         v .: "repo" <*>
                         v .: "requires" <*>
                         fmap fixNull (v .: "suggests") <*>
                         v .: "summary" <*>
                         fmap fixNull (v .: "supplements") <*>
                         v .: "version"
    where
      fixNull = fromMaybe []
  parseJSON _          = mempty


--------------------------------------------------------------------------------
-- Changelog
--------------------------------------------------------------------------------

data ChangelogEntry =
  ChangelogEntry { _author :: T.Text
                 , _log :: T.Text
                 , _date :: Integer
                 } deriving (Eq, Show)

makeLenses ''ChangelogEntry

instance FromJSON ChangelogEntry where
  parseJSON (Object v) = ChangelogEntry <$>
                         v .: "author" <*>
                         v .: "changelog" <*>
                         v .: "date"
  parseJSON _          = mempty

data ChangelogResponse =
  ChangelogResponse { _changelogResponseEntries :: [ChangelogEntry]
                    , _changelogResponseRepo :: String
                    } deriving (Eq, Show)

makeLenses ''ChangelogResponse

instance FromJSON ChangelogResponse where
  parseJSON (Object v) = ChangelogResponse <$>
                         v .: "changelogs" <*>
                         v .: "repo"
  parseJSON _          = mempty

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

data FileEntry =
  FileEntry { _dirname :: T.Text
            , _filenames :: T.Text
            , _filetypes :: T.Text
            } deriving (Eq, Show)

makeLenses ''FileEntry

instance FromJSON FileEntry where
  parseJSON (Object v) = FileEntry <$>
                         v .: "dirname" <*>
                         v .: "filenames" <*>
                         v .: "filetypes"
  parseJSON _          = mempty

data FilesResponse =
  FilesResponse { _filesResponseEntries :: [FileEntry]
                , _filesResponseRepo :: String
                } deriving (Eq, Show)

makeLenses ''FilesResponse

instance FromJSON FilesResponse where
  parseJSON (Object v) = FilesResponse <$>
                         v .: "files" <*>
                         v .: "repo"
  parseJSON _          = mempty
