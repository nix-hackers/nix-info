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
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

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

nixQuery :: Text -> Shell (Maybe PackageList)
nixQuery arg =
  procStrict "nix-env" ["-qa", arg, "--json" ] empty >>= \case
  (ExitSuccess,txt) -> pure $ decode (cs txt)
  (status,_)        -> exit status

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main =
  sh $ arguments >>= \case
  [arg] -> nixQuery arg >>= \case
           Just (PackageList pkgs) -> liftIO $ for_ pkgs printPackage
           Nothing                 -> exit $ ExitFailure 1
  _     -> do echo "TODO: usage"
              exit $ ExitFailure 1

-- --------------------------------------------------------------------- [ EOF ]
