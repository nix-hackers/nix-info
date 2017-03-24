#line 268 "nix-info.nw"
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

#line 423 "nix-info.nw"
import           Prelude                 hiding (putStrLn)

#line 458 "nix-info.nw"
import           Data.Foldable           (traverse_)
import           Data.Maybe              (catMaybes)

#line 287 "nix-info.nw"
import qualified Data.Text               as T
#line 453 "nix-info.nw"
import           Data.Text.IO            (putStrLn)

#line 290 "nix-info.nw"
import           Network.URL             (exportURL)

#line 248 "nix-info.nw"
-- printPackage :: MonadIO io => Package -> io ()
printPackage :: Package -> IO ()
printPackage (Package pkgPath (PackageInfo pkgName _pkgSystem pkgMeta)) =
  traverse_ putStrLn $
  catMaybes
  [ Just pkgName
  -- , Just pkgSystem
  , description pkgMeta
  , T.pack . exportURL . (\(NixURL url) -> url) <$> homepage pkgMeta
  -- , T.unwords . T.words <$> longDescription pkgMeta
  , T.unwords <$> maintainers pkgMeta
  -- , T.unwords <$> outputsToInstall pkgMeta
  -- , T.unwords <$> platforms pkgMeta
  , Just pkgPath
  , position pkgMeta
  ]
