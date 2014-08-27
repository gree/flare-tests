-- author: Benjamin Surma <benjamin.surma@gree.net>

import Test.Framework

import FailoverTests (failoverTests)
import ProtocolTests (protocolTests)
import Properties (properties)
import ReplicationTests (replicationTests)
import Options

import System.Console.CmdArgs
import System.FilePath

main :: IO ()
main = do
  options <- cmdArgs defaultOptions
  let binDir = flare_bin_dir options
  defaultMainWithArgs [ (failoverTests binDir)
                        , (protocolTests binDir)
                        , (properties binDir)
                        , (replicationTests binDir) ] []
