-- author: Benjamin Surma <benjamin.surma@gree.net>

module ReplicationTests (replicationTests) where

import Test.Framework
import Test.Framework.Providers.Sandbox (sandboxTests, sandboxTest, sandboxTestGroup, sandboxTestGroup')
import Test.Sandbox (Sandbox, liftIO)
import Test.Sandbox.HUnit (assertEqual, assertException)

import Main.Internals

import Control.Monad
import Data.Time.Clock.POSIX
import GHC.Conc

replicationTests :: Test
replicationTests = sandboxTests "flare" $ setup >> sandboxTestGroup "replication tests" [
    sandboxTest "flared setup" setupFlareCluster
  , sandboxTest "expiration bug" expirationTest
  ]

expirationTest :: Sandbox ()
expirationTest = do
  let master   = FlareDaemon 0 Master
      slave    = FlareDaemon 0 (Slave 0)
      masterId = fdId master
      slaveId  = fdId slave
  assertSendTo masterId "set key 0 0 5\r\nvalue\r\n" "STORED\r\n"
  assertSendTo masterId "gets key\r\n" "VALUE key 0 5 1\r\nvalue\r\nEND\r\n"
  assertSendTo slaveId "gets key\r\n" "VALUE key 0 5 1\r\nvalue\r\nEND\r\n"
  assertSendTo masterId "set key 0 1 7\r\nvalue_2\r\n" "STORED\r\n"
  liftIO $ threadDelay 3000000 -- wait 3.
  assertSendTo masterId "get key\r\n" "END\r\n"
  assertSendTo masterId "set key 0 0 7\r\nvalue_3\r\n" "STORED\r\n"
  assertSendTo masterId "gets key\r\n" "VALUE key 0 7 3\r\nvalue_3\r\nEND\r\n"
  assertSendTo slaveId "gets key\r\n" "VALUE key 0 7 3\r\nvalue_3\r\nEND\r\n"
