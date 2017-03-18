-- ---------------------------------------------------------------- [ Types.hs ]
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
------------------------------------------------------------------------ [ EOH ]
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module NixInfo.Types where

import           Data.Aeson
import           Data.Aeson.TH     (defaultOptions, deriveJSON)

import qualified Data.HashMap.Lazy as HM
import           Data.Text         (Text)

-- -------------------------------------------------------------- [ Data Types ]

data Meta = Meta
  { description      :: Maybe Text
  , homepage         :: Maybe Text
  , longDescription  :: Maybe Text
  , maintainers      :: Maybe [Text]
  , outputsToInstall :: Maybe [Text]
  , platforms        :: Maybe [Text]
  , position         :: Maybe Text
  }
  deriving (Show)

data PackageInfo = PackageInfo
  { name   :: Text
  , system :: Text
  , meta   :: Meta
  }
  deriving (Show)

data Package = Package
  { path :: Text
  , info :: PackageInfo
  }
  deriving (Show)

newtype PackageList = PackageList [Package]

-- ------------------------------------------------------ [ FromJSON Instances ]

$(deriveJSON defaultOptions ''Meta)

$(deriveJSON defaultOptions ''PackageInfo)

instance FromJSON PackageList where
  parseJSON (Object v) =
    PackageList <$> traverse (\(p,y) -> Package p <$> parseJSON y) (HM.toList v)
  parseJSON _          = fail "non-object"

-- --------------------------------------------------------------------- [ EOF ]
