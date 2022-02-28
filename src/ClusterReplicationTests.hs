-- author: Benjamin Surma <benjamin.surma@gree.net>

module ClusterReplicationTests (clusterReplicationTests) where

import Test.Framework
import Test.Framework.Providers.Sandbox (sandboxTests, sandboxTest, sandboxTestGroup, yieldProgress)
import Test.Sandbox

import GHC.Conc
import System.FilePath
import System.Directory
import Control.Monad (void)

import Main.Internals (assertSendTo, getFlareRoot)

registerNode :: String -> String -> String -> Sandbox ()
registerNode indexName daemonName dir = do
  liftIO $ createDirectory (dir ++ "/" ++ daemonName)
  bin <- getVariable "flared_bin" "flared"
  flareiPort <- getPort indexName
  port <- getPort daemonName
  void $ register daemonName bin [ "--data-dir", (dir ++ "/" ++ daemonName)
                                 , "--index-server-name", "localhost"
                                 , "--index-server-port", show flareiPort
                                 , "--replication-type", "sync"
                                 , "--server-name", "localhost"
                                 , "--server-port", show port
                                 ] def { psWait = Nothing }

registerReplicaNode :: String -> String -> String -> String -> String -> Sandbox ()
registerReplicaNode indexName daemonName dir backwardServerName replicationMode = do
  liftIO $ createDirectory (dir ++ "/" ++ daemonName)
  bin <- getVariable "flared_bin" "flared"
  flareiPort <- getPort indexName
  port <- getPort daemonName
  backwardPort <- getPort backwardServerName
  void $ register daemonName bin [ "--data-dir", (dir ++ "/" ++ daemonName)
                                 , "--index-server-name", "localhost"
                                 , "--index-server-port", show flareiPort
                                 , "--replication-type", "sync"
                                 , "--server-name", "localhost"
                                 , "--server-port", show port
                                 , "--cluster-replication", "true"
                                 , "--cluster-replication-server-name", "localhost"
                                 , "--cluster-replication-server-port", show backwardPort
                                 , "--cluster-replication-concurrency", "2"
                                 , "--cluster-replication-mode", replicationMode
                                 ] def { psWait = Nothing }



registerIndex :: String -> String -> Sandbox ()
registerIndex indexName dir = do
  liftIO $ createDirectory (dir ++ "/" ++ indexName)
  bin <- getVariable "flarei_bin" "flarei"
  flareiPort <- getPort indexName
  void $ register indexName bin [ "--data-dir", dir ++ "/" ++ indexName
                                , "--server-name", "localhost"
                                , "--server-port", show flareiPort
                                , "--monitor-interval", "1"
                                , "--monitor-threshold", "2"
                                ] def


setupWithPath :: Maybe FilePath -> Sandbox ()
setupWithPath binDir = do
  -- Register index server
  rootDir <- liftIO getFlareRoot
  let flareiBin = case (binDir, rootDir) of
                    (Just v, _) -> v </> "flarei"
                    (_, Just v) -> v </> "src" </> "flarei" </> "flarei"
                    (_, Nothing) -> "/usr" </> "local" </> "bin" </> "flarei"
      flaredBin = case (binDir, rootDir) of
                    (Just v, _) -> v </> "flared"
                    (_, Just v) -> v </> "src" </> "flared" </> "flared"
                    (_, Nothing) -> "/usr" </> "local" </> "bin" </> "flared"
  setVariable "flarei_bin" flareiBin
  setVariable "flared_bin" flaredBin

  dataDir <- getDataDir
  registerIndex "backward-flarei" dataDir
  registerNode "backward-flarei" "backward-master-0" dataDir
  registerIndex "forward-flarei" dataDir
  registerReplicaNode "forward-flarei" "forward-master-0" dataDir "backward-master-0" "forward"
  startAll
  liftIO $ threadDelay 1000000
  setupFlareDaemon "backward-flarei" "backward-master-0"
  setupFlareDaemon "forward-flarei" "forward-master-0"
  liftIO $ threadDelay 1000000

setupFlareDaemon :: String -> String -> Sandbox ()
setupFlareDaemon indexName nodeName = do
  yieldProgress $ "Setting up " ++ indexName
  port <- getPort nodeName
  void $ assertSendTo indexName ("node role " ++ "localhost " ++ show port ++ " master 1 0\r\n") "OK\r\n"

clusterReplicationTests :: Maybe FilePath -> Test
clusterReplicationTests binDir = sandboxTests "cluster-replication" [
    sandboxTest "flared setup" $ setupWithPath binDir
  , sandboxTest "1. forward bug" forwardTest
  ]

forwardTest :: Sandbox ()
forwardTest = do
  let forward = "forward-master-0"
      backward  = "backward-master-0"
  assertSendTo backward "set 1::key 0 0 5\r\nvalue\r\n" "STORED\r\n"
  assertSendTo forward "gets 1::key\r\n" "VALUE 1::key 0 5 1\r\nvalue\r\nEND\r\n"
  assertSendTo backward "gets 1::key\r\n" "VALUE 1::key 0 5 1\r\nvalue\r\nEND\r\n"
  assertSendTo forward "cas 1::key 0 0 5 1\r\nValue\r\n" "STORED\r\n"
  assertSendTo forward "gets 1::key\r\n" "VALUE 1::key 0 5 2\r\nValue\r\nEND\r\n"
  assertSendTo backward "gets 1::key\r\n" "VALUE 1::key 0 5 2\r\nValue\r\nEND\r\n"
  assertSendTo forward "delete 1::key\r\n" "DELETED\r\n"
  assertSendTo forward "gets 1::key\r\n" "END\r\n"
  assertSendTo backward "gets 1::key\r\n" "END\r\n"
  liftIO $ threadDelay 3000000 -- wait 3.
