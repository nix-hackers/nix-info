#line 209 "nix-info.nw"
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
#line 89 "nix-info.nw"
{-# LANGUAGE OverloadedStrings #-}
#line 100 "nix-info.nw"
{-# LANGUAGE TemplateHaskell   #-}

#line 224 "nix-info.nw"
module NixInfo.Types where

#line 106 "nix-info.nw"
import           Data.Aeson
import           Data.Aeson.TH     (defaultOptions, deriveJSON)

import qualified Data.HashMap.Lazy as HM
import           Data.Text         (Text)

#line 116 "nix-info.nw"
-- -------------------------------------------------------------- [ Data Types ]

#line 138 "nix-info.nw"
data Meta = Meta
  { description      :: Maybe Text
  , longDescription  :: Maybe Text
  , branch           :: Maybe Text
  , homepage         :: Maybe Text
  , downloadPage     :: Maybe Text
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

#line 160 "nix-info.nw"
data PackageInfo = PackageInfo
  { name   :: Text
  , system :: Text
  , meta   :: Meta
  }
  deriving (Show)

#line 171 "nix-info.nw"
data Package = Package
  { path :: Text
  , info :: PackageInfo
  }
  deriving (Show)

#line 184 "nix-info.nw"
newtype PackageList = PackageList [Package]

#line 198 "nix-info.nw"
-- ------------------------------------------------------ [ FromJSON Instances ]

#line 190 "nix-info.nw"
$(deriveJSON defaultOptions ''Meta)

$(deriveJSON defaultOptions ''PackageInfo)

#line 202 "nix-info.nw"
instance FromJSON PackageList where
  parseJSON (Object v) =
    PackageList <$> traverse (\(p,y) -> Package p <$> parseJSON y) (HM.toList v)
  parseJSON _          = fail "non-object"

#line 232 "nix-info.nw"
-- --------------------------------------------------------------------- [ EOF ]
