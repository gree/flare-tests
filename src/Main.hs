-- author: Benjamin Surma <benjamin.surma@gree.net>

import Test.Framework
import Test.Framework.Providers.Sandbox (sandboxTests, sandboxTest, sandboxTestGroup, sandboxTestGroup', yieldProgress)
import Test.Sandbox (Sandbox, liftIO, getVariable, silently, stop, signal)
import Test.Sandbox.HUnit (assertBool, assertEqual, assertException, assertFailure)
import Test.Sandbox.QuickCheck
import Test.QuickCheck (suchThat)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic

import Main.Internals

import Control.Monad
import Data.Char
import Data.Time.Clock.POSIX
import GHC.Conc
import Text.Printf
import System.Posix.Signals
import Data.List.Split

main :: IO ()
main = do
  defaultMain [ failoverTests, protocolTests ]

failoverTests = testGroup "failover" $ take 10 $ repeat failoverTests'

failoverTests' = do
  let
      (~=>) :: String -> String -> Sandbox ()
      i ~=> o = void $ assertSendToDaemon i o
      numOfActiveNodes resp = (length $ filter (\x -> "active" `elem` splitOn " " x) $ splitOn "\r\n" resp)
      check = withTimeout 1000 $ do
        resp <- sendTo "flarei" "stats nodes\r\n"
        when (resp == "") $ assertFailure "flarei does not respond"
  sandboxTests "monitor_failover" $ setup >> sandboxTestGroup "all" [
      sandboxTest "1. flared setup" setupFlareCluster
    , sandboxTest "2. stop" $ signal "flarei" sigSTOP
    , sandboxTest "3. kill" $ mapM_ (flip signal sigKILL) [ "0_Master", "0_Slave_1" ]
    , sandboxTest "4. wait 100ms" $ liftIO $ threadDelay (100000)
    , sandboxTest "5. cont" $ signal "flarei" sigCONT
    , sandboxTest "6. wait" $ liftIO $ threadDelay (6*1000000)
    , sandboxTest "7. ping" $ assertSendTo "flarei" "ping\r\n" "OK\r\n"
    , sandboxTest "8. check" $ check
    , sandboxTest "9. stop" $ signal "flarei" sigSTOP
    , sandboxTest "10. kill" $ mapM_ (flip signal sigKILL) [ "1_Master", "1_Slave_1" ]
    , sandboxTest "11. wait 100ms" $ liftIO $ threadDelay (100000)
    , sandboxTest "12. cont" $ signal "flarei" sigCONT
    , sandboxTest "13. wait" $ liftIO $ threadDelay (6*1000000)
    , sandboxTest "14. ping" $ assertSendTo "flarei" "ping\r\n" "OK\r\n"
    , sandboxTest "15. check" $ check
    , sandboxTest "16. stop" $ signal "flarei" sigSTOP
    , sandboxTest "17. kill" $ mapM_ (flip signal sigKILL) [ "2_Slave_1", "2_Master" ]
    , sandboxTest "18. wait 100ms" $ liftIO $ threadDelay (100000)
    , sandboxTest "19. cont" $ signal "flarei" sigCONT
    , sandboxTest "20. wait" $ liftIO $ threadDelay (8*1000000)
    , sandboxTest "21. ping" $ assertSendTo "flarei" "ping\r\n" "OK\r\n"
    , sandboxTest "22. check" $ check
    ]

protocolTests = do
  let
      (~=>) :: String -> String -> Sandbox ()
      i ~=> o = void $ assertSendToDaemon i o
      maxInt32 :: Integer
      maxInt32 = 2^(32 :: Integer)
      maxInt64 :: Integer
      maxInt64 = 2^(64 :: Integer)
  sandboxTests "flare" $ setup >> sandboxTestGroup "all" [
      sandboxTestGroup "Pre-initialization tests" [
        sandboxTest "ping" $ "ping\r\n" ~=> "OK\r\n"
      , sandboxTest "empty" $ "\r\n" ~=> "ERROR\r\n"
      , sandboxTest "garbage" $ "!@#$%^&*()_+\r\n" ~=> "ERROR\r\n"
      , sandboxTest "get not initialized" $ "get key\r\n" ~=> "END\r\n"
      , sandboxTest "multi-get not initialized" $ "get a b c\r\n" ~=> "END\r\n"
      , sandboxTest "set not initialized" $ "set key 5 3600 5\r\nvalue\r\n" ~=> "SERVER_ERROR no partition available\r\n"
      ]
    -- Initialize the first partition
    , sandboxTest "flared setup" setupFlareCluster
    , sandboxTestGroup "Storage tests" [
      -- Basic flare tests
        sandboxTestGroup "Basic get/set tests" [
          sandboxTest "1. get inexistent" $ "get gs:key\r\n" ~=> "END\r\n"
        , sandboxTest "2. multi-get inexistent" $ "get gs:a gs:b gs:c\r\n" ~=> "END\r\n"
        , sandboxTest "3. set key value" $ "set gs:key 5 3600 5\r\nvalue\r\n" ~=> "STORED\r\n"
        , sandboxTest "4. get key" $ "get gs:key\r\n" ~=> "VALUE gs:key 5 5\r\nvalue\r\nEND\r\n"
        , sandboxTest "5. gets key" $ "gets gs:key\r\n" ~=> "VALUE gs:key 5 5 1\r\nvalue\r\nEND\r\n"
        , sandboxTest "6. set a b c" $ "set gs:a 5 3600 1\r\n\
                                       \1\r\n\
                                       \set gs:b 5 3600 1\r\n\
                                       \2\r\n\
                                       \set gs:c 5 3600 1\r\n\
                                       \3\r\n"
                                       ~=> "STORED\r\n\
                                           \STORED\r\n\
                                           \STORED\r\n"
        , sandboxTest "7. wait 200ms" $ liftIO $ threadDelay 200000
        -- Since the request is separated between the 2 nodes, we may get the keys in random order
        , sandboxTest "8. multi-get" $ do r <- sendToDaemon "get gs:a gs:1 gs:b gs:2 gs:c gs:3\r\n"
                                          assertEqual "get gs:a gs:1 gs:b gs:2 gs:c gs:3"
                                            (normalize r)
                                            (normalize "VALUE gs:a 5 1\r\n\
                                                       \1\r\n\
                                                       \VALUE gs:b 5 1\r\n\
                                                       \2\r\n\
                                                       \VALUE gs:c 5 1\r\n\
                                                       \3\r\n\
                                                       \END\r\n")
        , sandboxTest "9. multi-set" $ "set gs:a 5 3600 1 noreply\r\n\
                                       \1\r\n\
                                       \set gs:b 5 3600 1 noreply\r\n\
                                       \2\r\n\
                                       \set gs:c 5 3600 1\r\n\
                                       \3\r\n"
                                       ~=> "STORED\r\n"
        ]
      -- Memcached test suite
      -- https://github.com/memcached/memcached/blob/master/t/getset.t
      , sandboxTestGroup "memcached get/set tests" [
          sandboxTest "1. set key_a" $ "set mgs:key_a 0 0 7\r\nvalue_a\r\n" ~=> "STORED\r\n"
        , sandboxTest "2. get key_a" $ "get mgs:key_a\r\n" ~=> "VALUE mgs:key_a 0 7\r\nvalue_a\r\nEND\r\n"
        , sandboxTest "3. add key_b" $ "add mgs:key_b 0 0 7\r\nvalue_b\r\n" ~=> "STORED\r\n"
        , sandboxTest "4. get key_b" $ "get mgs:key_b\r\n" ~=> "VALUE mgs:key_b 0 7\r\nvalue_b\r\nEND\r\n"
        , sandboxTest "5. add key_a (should fail)" $ "add mgs:key_a 0 0 10\r\nvalue_a_v2\r\n" ~=> "NOT_STORED\r\n"
        , sandboxTest "6. get key_a" $ "get mgs:key_a\r\n" ~=> "VALUE mgs:key_a 0 7\r\nvalue_a\r\nEND\r\n"
        , sandboxTest "7. replace key_b" $ "replace mgs:key_b 0 0 10\r\nvalue_b_v2\r\n" ~=> "STORED\r\n"
        , sandboxTest "8. replace key_z (should fail)" $ "replace mgs:key_z 0 0 7\r\nvalue_z\r\n" ~=> "NOT_STORED\r\n"
        , sandboxTest "9. delete key_a" $ "delete mgs:key_a\r\n" ~=> "DELETED\r\n"
        , sandboxTest "10. delete key_a again (should fail)" $ "delete mgs:key_a\r\n" ~=> "NOT_FOUND\r\n"
        , sandboxTest "11. add key_c" $ "add mgs:key_c 0 0 7\r\nvalue_c\r\n" ~=> "STORED\r\n"
        , sandboxTest "12. get key_c" $ "get mgs:key_c\r\n" ~=> "VALUE mgs:key_c 0 7\r\nvalue_c\r\nEND\r\n"
        , sandboxTest "13. cas key_c (should fail)" $ "cas mgs:key_c 0 0 10 0\r\nvalue_c_v2\r\n" ~=> "EXISTS\r\n"
        , sandboxTest "14. gets key_c" $ "gets mgs:key_c\r\n" ~=> "VALUE mgs:key_c 0 7 1\r\nvalue_c\r\nEND\r\n"
        , sandboxTest "15. cas key_c" $ "cas mgs:key_c 0 0 10 1\r\nvalue_c_v3\r\n" ~=> "STORED\r\n"
        , sandboxTest "16. get key_c" $ "get mgs:key_c\r\n" ~=> "VALUE mgs:key_c 0 10\r\nvalue_c_v3\r\nEND\r\n"
        , sandboxTest "17. pipelining" $ "set mgs:key_a 0 0 7\r\nvalue_a\r\ndelete mgs:key_a\r\nset mgs:key_a 0 0 7\r\nvalue_b\r\ndelete mgs:key_a\r\n"
                                         ~=> "STORED\r\n\
                                             \DELETED\r\n\
                                             \STORED\r\n\
                                             \DELETED\r\n"
        ]
      -- https://github.com/memcached/memcached/blob/master/t/expirations.t
      , sandboxTestGroup "memcached expiration tests" [
          sandboxTest "1. set key_a (relative)" $ "set me:key_a 0 2 7\r\nvalue_a\r\n" ~=> "STORED\r\n"
        , sandboxTest "2. get key_a" $ "get me:key_a\r\n" ~=> "VALUE me:key_a 0 7\r\nvalue_a\r\nEND\r\n"
        , sandboxTest "3. wait 2.5s" $ liftIO $ threadDelay 2500000
        , sandboxTest "4. get key_a (expired)" $ "get me:key_a\r\n" ~=> "END\r\n"
        , sandboxTest "5. set key_a (absolute)" $ do i <- liftM floor $ liftIO getPOSIXTime :: Sandbox Integer
                                                     ("set me:key_a 0 " ++ show (i - 1) ++ " 7\r\nvalue_a\r\n") ~=> "STORED\r\n"
        , sandboxTest "6. get key_a (already expired)" $ "get me:key_a\r\n" ~=> "END\r\n"
        , sandboxTest "7. set key_a (absolute)" $ do i <- liftM floor $ liftIO getPOSIXTime :: Sandbox Integer
                                                     ("set me:key_a 0 " ++ show (i + 1) ++ " 10\r\nvalue_a_+1\r\n") ~=> "STORED\r\n"
        , sandboxTest "8. get key_a" $ "get me:key_a\r\n" ~=> "VALUE me:key_a 0 10\r\nvalue_a_+1\r\nEND\r\n"
        , sandboxTest "9. wait 1.5s" $ liftIO $ threadDelay 1500000
        , sandboxTest "10. get key_a (expired)" $ "get me:key_a\r\n" ~=> "END\r\n"
        , sandboxTest "11. add key_b" $ "add me:key_b 0 2 7\r\nvalue_b\r\n" ~=> "STORED\r\n"
        , sandboxTest "12. get key_b" $ "get me:key_b\r\n" ~=> "VALUE me:key_b 0 7\r\nvalue_b\r\nEND\r\n"
        , sandboxTest "13. add key_b (should fail)" $ "add me:key_b 0 2 10\r\nvalue_b_v2\r\n" ~=> "NOT_STORED\r\n"
        , sandboxTest "14. wait 2.5s" $ liftIO $ threadDelay 2500000
        , sandboxTest "15. add key_b (should work)" $ "add me:key_b 0 2 10\r\nvalue_b_v3\r\n" ~=> "STORED\r\n"
        , sandboxTest "16. get key_b" $ "get me:key_b\r\n" ~=> "VALUE me:key_b 0 10\r\nvalue_b_v3\r\nEND\r\n"
        ]
      -- https://github.com/memcached/memcached/blob/master/t/incrdecr.t
      , sandboxTestGroup "memcached incr/decr tests" [
          sandboxTest "1. bug 21" $ "set mid:bug21 0 0 19\r\n9223372036854775807\r\n" ~=> "STORED\r\n"
        , sandboxTest "2. incr bug21 1" $ "incr mid:bug21 1\r\n" ~=> "9223372036854775808\r\n"
        , sandboxTest "3. incr bug21 1" $ "incr mid:bug21 1\r\n" ~=> "9223372036854775809\r\n"
        , sandboxTest "4. decr bug21 1" $ "decr mid:bug21 1\r\n" ~=> "9223372036854775808\r\n"
        -- Simple tests
        , sandboxTest "5. set key_a" $ "set mid:key_a 0 0 1\r\n1\r\n" ~=> "STORED\r\n"
        , sandboxTest "6. get key_a" $ "get mid:key_a\r\n" ~=> "VALUE mid:key_a 0 1\r\n1\r\nEND\r\n"
        , sandboxTest "7. incr key_a 1" $ "incr mid:key_a 1\r\n" ~=> "2\r\n"
        , sandboxTest "8. get key_a" $ "get mid:key_a\r\n" ~=> "VALUE mid:key_a 0 1\r\n2\r\nEND\r\n"
        , sandboxTest "9. incr key_a 8" $ "incr mid:key_a 8\r\n" ~=> "10\r\n"
        , sandboxTest "10. get key_a" $ "get mid:key_a\r\n" ~=> "VALUE mid:key_a 0 2\r\n10\r\nEND\r\n"
        , sandboxTest "11. decr key_a 1" $ "decr mid:key_a 1\r\n" ~=> "9\r\n"
        , sandboxTest "12. get key_a" $ "get mid:key_a\r\n" ~=> "VALUE mid:key_a 0 1\r\n9\r\nEND\r\n"
        , sandboxTest "13. decr key_a 9" $ "decr mid:key_a 9\r\n" ~=> "0\r\n"
        , sandboxTest "14. get key_a" $ "get mid:key_a\r\n" ~=> "VALUE mid:key_a 0 1\r\n0\r\nEND\r\n"
        , sandboxTest "15. decr key_a 5" $ "decr mid:key_a 5\r\n" ~=> "0\r\n"
        , sandboxTest "16. get key_a" $ "get mid:key_a\r\n" ~=> "VALUE mid:key_a 0 1\r\n0\r\nEND\r\n"
        -- Overflow
        , sandboxTest "17. store 2^32" $ ("set mid:key_a 0 0 " ++ show (length $ show maxInt32) ++ "\r\n" ++ show maxInt32 ++ "\r\n") ~=> "STORED\r\n"
        , sandboxTest "18. get key_a" $ "get mid:key_a\r\n" ~=> ("VALUE mid:key_a 0 " ++ show (length $ show maxInt32) ++ "\r\n" ++ show maxInt32 ++ "\r\nEND\r\n")
        , sandboxTest "19. incr key_a 1" $ "incr mid:key_a 1\r\n" ~=> (show (maxInt32 + 1) ++ "\r\n")
        , sandboxTest "20. store 2^64" $ ("set mid:key_a 0 0 " ++ show (length $ show maxInt64) ++ "\r\n" ++ show maxInt64 ++ "\r\n") ~=> "STORED\r\n"
        , sandboxTest "21. get key_a" $ "get mid:key_a\r\n" ~=> ("VALUE mid:key_a 0 " ++ show (length $ show maxInt64) ++ "\r\n" ++ show maxInt64 ++ "\r\nEND\r\n")
        , sandboxTest "22. incr key_a 1" $ "incr mid:key_a 1\r\n" ~=> "1\r\n" -- Flare-specific bug, should be 0
        -- Limit cases
        , sandboxTest "23. decr bogus" $ "decr mid:bogus 5\r\n" ~=> "NOT_FOUND\r\n"
        , sandboxTest "24. incr bogus" $ "incr mid:bogus 5\r\n" ~=> "NOT_FOUND\r\n"
        , sandboxTest "25. set bignum" $ "set mid:bignum 0 0 1\r\n0\r\n" ~=> "STORED\r\n"
        , sandboxTest "26. incr bignum 2^64-1" $ ("incr mid:bignum " ++ show (maxInt64 - 1) ++ "\r\n") ~=> (show (maxInt64 - 1) ++ "\r\n")
        , sandboxTest "27. set text" $ "set mid:text 0 0 2\r\nhi\r\n" ~=> "STORED\r\n"
        , sandboxTest "28. incr text" $ "incr mid:text 1\r\n" ~=> "1\r\n" -- Flare-specific bug, should return a CLIENT_ERROR
        ]
      -- https://github.com/memcached/memcached/blob/master/t/noreply.t
      , sandboxTestGroup "memcached noreply tests" [
          sandboxTest "1. add" $ assertException "no reply" $ "add nr:key 0 0 1 noreply\r\n1\r\n" ~=> ""
        , sandboxTest "2. check" $ "get nr:key\r\n" ~=> "VALUE nr:key 0 1\r\n1\r\nEND\r\n"
        , sandboxTest "3. set" $ assertException "no reply" $ "set nr:key 0 0 1 noreply\r\n2\r\n" ~=> ""
        , sandboxTest "4. check" $ "get nr:key\r\n" ~=> "VALUE nr:key 0 1\r\n2\r\nEND\r\n"
        , sandboxTest "5. replace" $ assertException "no reply" $ "replace nr:key 0 0 1 noreply\r\n3\r\n" ~=> ""
        , sandboxTest "6. check" $ "get nr:key\r\n" ~=> "VALUE nr:key 0 1\r\n3\r\nEND\r\n"
        , sandboxTest "7. append" $ assertException "no reply" $ "append nr:key 0 0 1 noreply\r\n4\r\n" ~=> ""
        , sandboxTest "8. check" $ "get nr:key\r\n" ~=> "VALUE nr:key 0 2\r\n34\r\nEND\r\n"
        , sandboxTest "9. prepend" $ assertException "no reply" $ "prepend nr:key 0 0 1 noreply\r\n5\r\n" ~=> ""
        , sandboxTest "10. check" $ "gets nr:key\r\n" ~=> "VALUE nr:key 0 3 5\r\n534\r\nEND\r\n"
        , sandboxTest "11. cas" $ assertException "no reply" $ "cas nr:key 0 0 1 5 noreply\r\n6\r\n" ~=> ""
        , sandboxTest "12. check" $ "get nr:key\r\n" ~=> "VALUE nr:key 0 1\r\n6\r\nEND\r\n"
        , sandboxTest "13. incr" $ assertException "no reply" $ "incr nr:key 3 noreply\r\n" ~=> ""
        , sandboxTest "14. check" $ "get nr:key\r\n" ~=> "VALUE nr:key 0 1\r\n9\r\nEND\r\n"
        , sandboxTest "15. decr" $ assertException "no reply" $ "decr nr:key 2 noreply\r\n" ~=> ""
        , sandboxTest "16. check" $ "get nr:key\r\n" ~=> "VALUE nr:key 0 1\r\n7\r\nEND\r\n"
        , sandboxTest "17. delete" $ assertException "no reply" $ "delete nr:key noreply\r\n" ~=> ""
        , sandboxTest "18. check" $ "get nr:key\r\n" ~=> "END\r\n"
        ]
      -- https://github.com/memcached/memcached/blob/master/t/touch.t
      , sandboxTestGroup' "memcached touch tests" (do version <- sendToDaemon "version\r\n"
                                                      return $ "VERSION flare-1.0.16\r\n" /= version) [
          sandboxTest "1. set" $ "set mt:key 0 1 5\r\nvalue\r\n" ~=> "STORED\r\n"
        , sandboxTest "2. touch" $ "touch mt:key 10\r\n" ~=> "TOUCHED\r\n"
        , sandboxTest "3. wait 2s" $ liftIO $ threadDelay 2000000
        , sandboxTest "4. get" $ "get mt:key\r\n" ~=> "VALUE mt:key 0 5\r\nvalue\r\nEND\r\n"
        ]
      , sandboxTestGroup "properties" [
          sandboxTest "set->get" $ quickCheck $ do k <- pick $ arbitrary `suchThat` (\s -> not (null s) && all isAlphaNum s) :: PropertyM Sandbox String
                                                   v <- pick arbitrary :: PropertyM Sandbox String
                                                   void $ run $ assertSendToDaemon (printf "set %s 0 0 %d\r\n%s\r\n" k (length v) v) "STORED\r\n"
                                                   void $ run $ assertSendToDaemon (printf "get %s\r\n" k) (printf "VALUE %s 0 %d\r\n%s\r\nEND\r\n" k (length v) v)
        , sandboxTest "set->incr" $ quickCheck $ do k <- pick $ arbitrary `suchThat` (\s -> not (null s) && all isAlphaNum s) :: PropertyM Sandbox String
                                                    x <- pick $ arbitrary `suchThat` (>= 0) :: PropertyM Sandbox Int
                                                    y <- pick $ arbitrary `suchThat` (> 0) :: PropertyM Sandbox Int
                                                    void $ run $ assertSendToDaemon (printf "set %s 0 0 %d\r\n%d\r\n" k (length $ show x) x) "STORED\r\n"
                                                    void $ run $ assertSendToDaemon (printf "incr %s %d\r\n" k y) (printf "%d\r\n" (x + y))
        ]
      , sandboxTestGroup "partitioning" [
          sandboxTest "repartition" $ do daemons <- getVariable "daemons" [] :: Sandbox [FlareDaemon]
                                         items <- mapM (liftM read . getStat "curr_items" :: FlareDaemon -> Sandbox Int) daemons
                                         let avg = fromIntegral (sum items) / fromIntegral (length items) :: Double
                                             dev = max (fromIntegral (maximum items) - avg) (avg - fromIntegral (minimum items)) / avg
                                         liftIO . putStrLn $ show items ++ " => Deviation: " ++ show dev
                                         assertBool "deviation too large" (dev < 0.3)
        ]
      , sandboxTestGroup "index server tests" [
          sandboxTest "2 nodes failover" $ do yieldProgress "Killing 2 daemons"
                                              daemons <- getVariable "daemons" [] :: Sandbox [FlareDaemon]
                                              mapM_ (Test.Sandbox.silently . Test.Sandbox.stop . fdId) (take 2 daemons)
                                              yieldProgress "Waiting 10 seconds"
                                              liftIO $ threadDelay 10000000
                                              yieldProgress "Sending ping to the index server"
                                              assertEqual "pong" "OK\r\n" =<< sendTo "flarei" "ping\r\n"
        ]
      ]
    ]
