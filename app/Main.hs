#line 317 "nix-info.nw"
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

#line 392 "nix-info.nw"
{-# LANGUAGE LambdaCase        #-}
#line 405 "nix-info.nw"
{-# LANGUAGE OverloadedStrings #-}

#line 331 "nix-info.nw"
module Main (main) where

import           NixInfo                 (printPackage)
import           NixInfo.Types

#line 427 "nix-info.nw"
import           Data.Aeson
#line 337 "nix-info.nw"
import           Data.Foldable           (traverse_)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)

#line 465 "nix-info.nw"
import           Turtle                  (ExitCode (..), Shell, arguments,
                                          echo, empty, exit, liftIO,
                                          procStrict, sh)

#line 298 "nix-info.nw"
nixQuery :: Text -> Shell (Maybe PackageList)
nixQuery arg =
  procStrict "nix-env" ["-qa", arg, "--json" ] empty >>= \case
  (ExitSuccess,txt) -> pure $ decode (cs txt)
  (status,_)        -> exit status

#line 306 "nix-info.nw"
main :: IO ()
main =
  sh $ arguments >>= \case
  [arg] -> nixQuery arg >>= \case
           Just (PackageList pkgs) -> liftIO $ traverse_ printPackage pkgs
           Nothing                 -> exit $ ExitFailure 1
  _     -> do echo "TODO: usage"
              exit $ ExitFailure 1
