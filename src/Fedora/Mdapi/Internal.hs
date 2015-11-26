{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : Fedora.Mdapi.Internal
-- Copyright : (C) 2015 Red Hat, Inc.
-- License : BSD2 (see LICENSE file)
-- Maintainer : Ricky Elrod <relrod@redhat.com>
-- Stability : experimental
-- Portability : ghc (lens)
--
-- Low-level access to mdapi.
----------------------------------------------------------------------------
module Fedora.Mdapi.Internal where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
--import Data.Aeson (toJSON)
--import Data.Aeson.Lens (key, nth)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (dropWhileEnd)
import Data.Monoid
import Network.Wreq
import Network.Wreq.Types (Postable)
import Fedora.Mdapi.Types

-- | Construct an API URL path. Strips any preceeding slashes from the given
-- 'String' parameter as well as the '_baseUrl' of the 'MdapiConfig'.
mdapiUrl :: String -> MdapiT String
mdapiUrl s = do
  MdapiConfig url <- ask
  return $ dropWhileEnd (== '/') url ++ "/" ++ dropWhile (== '/') s

mdapiWreqOptions :: Monad m => m Options
mdapiWreqOptions = return defaults

-- | Perform a @GET@ request to the API.
mdapiGetWith :: Options -> String -> MdapiT (Response BL.ByteString)
mdapiGetWith opts path = do
  path' <- mdapiUrl path
  liftIO $ getWith opts path'

-- | Perform a @GET@ request to the API with default options.
mdapiGet :: String -> MdapiT (Response BL.ByteString)
mdapiGet s = mdapiWreqOptions >>= flip mdapiGetWith s
