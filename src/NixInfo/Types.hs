#line 221 "nix-info.nw"
-- |
-- Module      : NixInfo.Types
-- Copyright   : (c) 2017, Eric Bailey
-- License     : BSD-style (see LICENSE)
--
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
--
-- Data types and JSON parsers for nix-info

#line 405 "nix-info.nw"
{-# LANGUAGE OverloadedStrings #-}
#line 413 "nix-info.nw"
{-# LANGUAGE TemplateHaskell   #-}

#line 235 "nix-info.nw"
module NixInfo.Types where

#line 427 "nix-info.nw"
import           Data.Aeson
#line 431 "nix-info.nw"
import           Data.Aeson.Encoding     (text)
#line 435 "nix-info.nw"
import           Data.Aeson.TH           (defaultOptions, deriveJSON)

#line 443 "nix-info.nw"
import qualified Data.HashMap.Lazy       as HM
import           Data.Text               (Text)
import qualified Data.Text               as T

import           Network.URL            (URL, exportURL, importURL)

#line 137 "nix-info.nw"
data Meta = Meta
  { description      :: Maybe Text
  , longDescription  :: Maybe Text
  , branch           :: Maybe Text
  , homepage         :: Maybe NixURL
  , downloadPage     :: Maybe NixURL
  , maintainers      :: Maybe [Text]
  , priority         :: Maybe Int
  , platforms        :: Maybe [Text]
  , hydraPlatforms   :: Maybe [Text]
  , broken           :: Maybe Bool
  , updateWalker     :: Maybe Bool
  , outputsToInstall :: Maybe [Text]
  , position         :: Maybe Text
  }
  deriving (Show)

#line 159 "nix-info.nw"
data PackageInfo = PackageInfo
  { name   :: Text
  , system :: Text
  , meta   :: Meta
  }
  deriving (Show)

#line 170 "nix-info.nw"
data Package = Package
  { path :: Text
  , info :: PackageInfo
  }
  deriving (Show)

#line 183 "nix-info.nw"
newtype PackageList = PackageList [Package]

#line 188 "nix-info.nw"
newtype NixURL = NixURL URL deriving (Show)

#line 194 "nix-info.nw"
$(deriveJSON defaultOptions ''Meta)

$(deriveJSON defaultOptions ''PackageInfo)

#line 204 "nix-info.nw"
instance FromJSON PackageList where
  parseJSON (Object v) =
    PackageList <$> traverse (\(p,y) -> Package p <$> parseJSON y) (HM.toList v)
  parseJSON _          = fail "non-object"

instance FromJSON NixURL where
  parseJSON (String t) = case importURL (T.unpack t) of
                           Just url -> pure $ NixURL url
                           Nothing  -> fail "no parse"
  parseJSON _          = fail "non-string"

instance ToJSON NixURL where
  toJSON (NixURL url)     = String (T.pack (exportURL url))
  toEncoding (NixURL url) = text (T.pack (exportURL url))
