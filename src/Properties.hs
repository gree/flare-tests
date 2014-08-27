-- author: Benjamin Surma <benjamin.surma@gree.net>

module Properties (properties) where

import Test.Framework
import Test.Framework.Providers.Sandbox (sandboxTests, sandboxTest, sandboxTestGroup)
import Test.Sandbox (Sandbox, liftIO, getVariable)
import Test.Sandbox.HUnit (assertBool)
import Test.Sandbox.QuickCheck
import Test.QuickCheck (suchThat)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic

import Main.Internals

import Control.Monad
import Data.Char
import Text.Printf
import System.FilePath

properties :: Maybe FilePath -> Test
properties binDir = sandboxTests "properties" [
    sandboxTest "setup" $ (setupWithPath binDir) >> setupFlareCluster
  , sandboxTestGroup "QuickCheck" [
      sandboxTest "set->get" $ quickCheck $ do k <- pick $ arbitrary `suchThat` (\s -> not (null s) && all isAlphaNum s) :: PropertyM Sandbox String
                                               v <- pick arbitrary :: PropertyM Sandbox String
                                               void $ run $ assertSendToDaemon (printf "set %s 0 0 %d\r\n%s\r\n" k (length v) v) "STORED\r\n"
                                               void $ run $ assertSendToDaemon (printf "get %s\r\n" k) (printf "VALUE %s 0 %d\r\n%s\r\nEND\r\n" k (length v) v)
    , sandboxTest "set->incr" $ quickCheck $ do k <- pick $ arbitrary `suchThat` (\s -> not (null s) && all isAlphaNum s) :: PropertyM Sandbox String
                                                x <- pick $ arbitrary `suchThat` (>= 0) :: PropertyM Sandbox Int
                                                y <- pick $ arbitrary `suchThat` (\v -> x + v > x) :: PropertyM Sandbox Int
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
  ]
