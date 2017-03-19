#line 244 "nix-info.nw"
-- |
-- Module      : NixInfo
-- Copyright   : (c) 2017, Eric Bailey
-- License     : BSD-style (see LICENSE)
--
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
--
-- brew info clone for Nix

module NixInfo (printPackage) where

import           NixInfo.Types

#line 414 "nix-info.nw"
import           Prelude                 hiding (putStrLn)

#line 442 "nix-info.nw"
import           Data.Foldable           (traverse_)
import           Data.Maybe              (catMaybes)

#line 263 "nix-info.nw"
import qualified Data.Text               as T
#line 437 "nix-info.nw"
import           Data.Text.IO            (putStrLn)

#line 270 "nix-info.nw"
-- printPackage :: MonadIO io => Package -> io ()
printPackage :: Package -> IO ()
printPackage (Package pkgPath (PackageInfo pkgName _pkgSystem pkgMeta)) =
  traverse_ putStrLn $
  catMaybes
  [ Just pkgName
  -- , Just pkgSystem
  , description pkgMeta
  , homepage pkgMeta
  -- , T.unwords . T.words <$> longDescription pkgMeta
  , T.unwords <$> maintainers pkgMeta
  -- , T.unwords <$> outputsToInstall pkgMeta
  -- , T.unwords <$> platforms pkgMeta
  , Just pkgPath
  , position pkgMeta
  ]
