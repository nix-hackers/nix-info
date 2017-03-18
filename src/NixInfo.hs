#line 244 "nix-info.nw"
-- -------------------------------------------------------------- [ NixInfo.hs ]
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
------------------------------------------------------------------------ [ EOH ]
module NixInfo (printPackage) where

import           NixInfo.Types

import           Prelude        ()
import           Prelude.Compat hiding (putStrLn)

import           Data.Foldable  (traverse_)
import           Data.Maybe     (catMaybes)

#line 239 "nix-info.nw"
import qualified Data.Text      as T
import           Data.Text.IO   (putStrLn)

#line 274 "nix-info.nw"
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

#line 270 "nix-info.nw"
-- --------------------------------------------------------------------- [ EOF ]
