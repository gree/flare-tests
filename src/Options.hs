-- author: Masanori Yoshimoto <masanori.yoshimoto@gree.net>

{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import System.Console.CmdArgs

{-# ANN module "HLint: ignore Use camelCase" #-}

data Options = Options {
    flare_bin_dir :: Maybe FilePath
 } deriving (Show, Data, Typeable)

defaultOptions :: Options
defaultOptions = Options {
    flare_bin_dir = Nothing &= typFile &= help "Directory path of flare binaries."
  } &= summary "flare-tests 0.0.1"
