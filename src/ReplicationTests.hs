-- author: Benjamin Surma <benjamin.surma@gree.net>

module ReplicationTests (replicationTests) where

import Test.Framework
import Test.Framework.Providers.Sandbox (sandboxTests, sandboxTest, sandboxTestGroup)
import Test.Sandbox (Sandbox, liftIO)

import Main.Internals

import GHC.Conc

replicationTests :: Test
replicationTests = sandboxTests "flare" $ setup >> sandboxTestGroup "replication" [
    sandboxTest "flared setup" setupFlareCluster
  , sandboxTest "1. expiration bug" expirationTest
  , sandboxTest "2. delete w/ expiration then CAS" deleteCASTest
  ]

expirationTest :: Sandbox ()
expirationTest = do
  let masterId = fdId $ FlareDaemon 0 Master
      slaveId  = fdId $ FlareDaemon 0 (Slave 0)
  assertSendTo masterId "set 1::key 0 0 5\r\nvalue\r\n" "STORED\r\n"
  assertSendTo masterId "gets 1::key\r\n" "VALUE 1::key 0 5 1\r\nvalue\r\nEND\r\n"
  assertSendTo slaveId "gets 1::key\r\n" "VALUE 1::key 0 5 1\r\nvalue\r\nEND\r\n"
  assertSendTo masterId "set 1::key 0 1 7\r\nvalue_2\r\n" "STORED\r\n"
  liftIO $ threadDelay 3000000 -- wait 3.
  assertSendTo masterId "get 1::key\r\n" "END\r\n"
  assertSendTo masterId "set 1::key 0 0 7\r\nvalue_3\r\n" "STORED\r\n"
  assertSendTo masterId "gets 1::key\r\n" "VALUE 1::key 0 7 3\r\nvalue_3\r\nEND\r\n"
  assertSendTo slaveId "gets 1::key\r\n" "VALUE 1::key 0 7 3\r\nvalue_3\r\nEND\r\n"

deleteCASTest :: Sandbox ()
deleteCASTest = do
  let masterId = fdId $ FlareDaemon 0 Master
      slaveId  = fdId $ FlareDaemon 0 (Slave 0)
  assertSendTo masterId "set 2::key 0 0 5\r\nvalue\r\n" "STORED\r\n"
  assertSendTo masterId "gets 2::key\r\n" "VALUE 2::key 0 5 1\r\nvalue\r\nEND\r\n"
  assertSendTo masterId "delete 2::key 3600\r\n" "DELETED\r\n"
  assertSendTo slaveId "cas 2::key 0 0 7 0\r\nvalue_2\r\n" "EXISTS\r\n"
  assertSendTo slaveId "cas 2::key 0 0 7 1\r\nvalue_3\r\n" "STORED\r\n"
  assertSendTo masterId "gets 2::key\r\n" "VALUE 2::key 0 7 2\r\nvalue_3\r\nEND\r\n"
