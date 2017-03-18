#line 314 "nix-info.nw"
-- ----------------------------------------------------------------- [ Main.hs ]
-- |
-- Module      : Main
-- Copyright   : (c) 2017, Eric Bailey
-- License     : BSD-style (see LICENSE)
--
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
--
-- Main executable for nix-info.
------------------------------------------------------------------------ [ EOH ]
#line 363 "nix-info.nw"
{-# LANGUAGE LambdaCase        #-}
#line 89 "nix-info.nw"
{-# LANGUAGE OverloadedStrings #-}

#line 329 "nix-info.nw"
module Main (main) where

import           NixInfo                 (printPackage)
import           NixInfo.Types

import           Control.Applicative     (empty)

import           Data.Aeson              (decode)
import           Data.Foldable           (for_)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)

import           Turtle                  (ExitCode (..), Shell, arguments, echo,
                                          exit, liftIO, procStrict, sh)

-- ----------------------------------------------------------- [ Private Parts ]

#line 306 "nix-info.nw"
nixQuery :: Text -> Shell (Maybe PackageList)
nixQuery arg =
  procStrict "nix-env" ["-qa", arg, "--json" ] empty >>= \case
  (ExitSuccess,txt) -> pure $ decode (cs txt)
  (status,_)        -> exit status

#line 348 "nix-info.nw"
-- -------------------------------------------------------------------- [ Main ]

#line 295 "nix-info.nw"
main :: IO ()
main =
  sh $ arguments >>= \case
  [arg] -> nixQuery arg >>= \case
           Just (PackageList pkgs) -> liftIO $ for_ pkgs printPackage
           Nothing                 -> exit $ ExitFailure 1
  _     -> do echo "TODO: usage"
              exit $ ExitFailure 1

#line 352 "nix-info.nw"
-- --------------------------------------------------------------------- [ EOF ]
