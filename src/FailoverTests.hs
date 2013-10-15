-- author: Kiyoshi Ikehara <kiyoshi.ikehara@gree.net>

module FailoverTests (failoverTests) where

import Test.Framework
import Test.Framework.Providers.Sandbox (sandboxTests, sandboxTest, sandboxTestGroup)
import Test.Sandbox (liftIO, signal)
import Test.Sandbox.HUnit (assertFailure)

import Main.Internals

import Control.Monad
import GHC.Conc
import System.Posix.Signals

failoverTests :: Test
failoverTests = do
  let
    check = withTimeout 1000 $ do
      resp <- sendTo "flarei" "stats nodes\r\n"
      when (resp == "") $ assertFailure "flarei does not respond"
  sandboxTests "failover" [
      sandboxTest "1. flared setup" $ setup >> setupFlareCluster
    , sandboxTest "2. stop" $ signal "flarei" sigSTOP
    , sandboxTest "3. kill" $ mapM_ (`signal` sigKILL) [ fdId (FlareDaemon 0 Master), fdId (FlareDaemon 0 $ Slave 0) ]
    , sandboxTest "4. wait 100ms" $ liftIO $ threadDelay 100000
    , sandboxTest "5. cont" $ signal "flarei" sigCONT
    , sandboxTest "6. wait" $ liftIO $ threadDelay (6*1000000)
    , sandboxTest "7. ping" $ assertSendTo "flarei" "ping\r\n" "OK\r\n"
    , sandboxTest "8. check" check
    , sandboxTest "9. stop" $ signal "flarei" sigSTOP
    , sandboxTest "10. kill" $ mapM_ (`signal` sigKILL) [ fdId (FlareDaemon 1 Master), fdId (FlareDaemon 1 $ Slave 0) ]
    , sandboxTest "11. wait 100ms" $ liftIO $ threadDelay 100000
    , sandboxTest "12. cont" $ signal "flarei" sigCONT
    , sandboxTest "13. wait" $ liftIO $ threadDelay (6*1000000)
    , sandboxTest "14. ping" $ assertSendTo "flarei" "ping\r\n" "OK\r\n"
    , sandboxTest "15. check" check
    , sandboxTest "16. stop" $ signal "flarei" sigSTOP
    , sandboxTest "17. kill" $ mapM_ (`signal` sigKILL) [ fdId (FlareDaemon 2 Master), fdId (FlareDaemon 2 $ Slave 0) ]
    , sandboxTest "18. wait 100ms" $ liftIO $ threadDelay 100000
    , sandboxTest "19. cont" $ signal "flarei" sigCONT
    , sandboxTest "20. wait" $ liftIO $ threadDelay (8*1000000)
    , sandboxTest "21. ping" $ assertSendTo "flarei" "ping\r\n" "OK\r\n"
    , sandboxTest "22. check" check
    ]
