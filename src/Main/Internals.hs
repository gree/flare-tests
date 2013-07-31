-- author: Benjamin Surma <benjamin.surma@gree.net>

{-# LANGUAGE DeriveGeneric #-}

module Main.Internals where

import Test.Sandbox hiding (sendTo)
import qualified Test.Sandbox (sendTo)
import Test.Sandbox.HUnit

import Control.Monad
import Data.List
import Data.Serialize
import GHC.Conc
import GHC.Generics (Generic)
import System.Directory
import System.FilePath
import System.Random

import Paths_flare_tests (getBinDir)

setup :: Sandbox ()
setup = do
  -- Register index server
  binDir <- liftIO getBinDir
  let flareiBin = binDir </> ".." </> ".." </> "flare" </> "src" </> "flarei" </> "flarei"
  dataDir <- getDataDir
  flareiPort <- getPort "flarei"
  register "flarei" flareiBin [ "--data-dir", dataDir
                              , "--server-name", "localhost"
                              , "--server-port", show flareiPort
                              , "--monitor-interval", "1"
                              , "--monitor-threshold", "2"
                              ] def
  -- Register daemons
  setVariable "flared_bin" $ binDir </> ".." </> ".." </> "flare" </> "src" </> "flared" </> "flared"
  daemons <- setVariable "daemons" [ FlareDaemon 0 Master
                                   , FlareDaemon 0 (Slave 0)
                                   , FlareDaemon 0 (Slave 1)
                                   , FlareDaemon 1 Master
                                   , FlareDaemon 1 (Slave 0)
                                   , FlareDaemon 1 (Slave 1)
                                   , FlareDaemon 2 Master
                                   , FlareDaemon 2 (Slave 0)
                                   , FlareDaemon 2 (Slave 1) ]
  mapM_ registerDaemon daemons
  startAll
  liftIO $ threadDelay 1000000

data FlareRole = Master
               | Slave Int
  deriving (Show, Generic)

instance Serialize FlareRole

data FlareDaemon = FlareDaemon {
    fdPartition :: Int
  , fdRole :: FlareRole }
  deriving (Show, Generic)

instance Serialize FlareDaemon

fdId :: FlareDaemon -> String
fdId fd = case fdRole fd of
  Master -> show (fdPartition fd) ++ "_Master"
  Slave i -> show (fdPartition fd) ++ "_Slave_" ++ show i

registerDaemon :: FlareDaemon -> Sandbox ()
registerDaemon fd = do
  dir <- getDataDir >>= (\d -> return $ d </> fdId fd)
  liftIO $ createDirectory dir
  bin <- getVariable "flared_bin" "flared"
  flareiPort <- getPort "flarei"
  port <- getPort (fdId fd)
  void $ register (fdId fd) bin [ "--data-dir", dir
                                , "--index-server-name", "localhost"
                                , "--index-server-port", show flareiPort
                                , "--replication-type", "sync"
                                , "--server-name", "localhost"
                                , "--server-port", show port
                                ] def { psWait = Nothing }

normalize :: String -> String
normalize = unlines . sort . lines

withTimeout :: Int -> Sandbox () -> Sandbox ()
withTimeout = withVariable "timeout"

sendTo :: String -> String -> Sandbox String
sendTo program input = do
  timeout <- getVariable "timeout" 250
  Test.Sandbox.sendTo program input timeout

sendToDaemon :: String -> Sandbox String
sendToDaemon input = do
  daemons <- getVariable "daemons" [] :: Sandbox [FlareDaemon]
  i <- liftIO randomIO :: Sandbox Int
  let fd = daemons !! (i `mod` length daemons)
  sendTo (fdId fd) input

assertSendTo :: String -> String -> String -> Sandbox ()
assertSendTo program input output =
  assertEqual input output =<< sendTo program input

assertSendToDaemon :: String -> String -> Sandbox ()
assertSendToDaemon input output =
  assertEqual input output =<< sendToDaemon input

getStat :: String -> FlareDaemon -> Sandbox String
getStat key fd = do
  stats <- sendTo (fdId fd) "stats\r\n"
  case find (key `isInfixOf`) $ lines stats of
    Nothing -> throwError $ "STAT " ++ key ++ " not found."
    Just stat -> return $ drop (length $ "STAT " ++ key ++ " ") stat

setupFlareDaemon :: FlareDaemon -> Sandbox ()
setupFlareDaemon fd = do
  port <- getPort (fdId fd)
  let hpId = "localhost " ++ show port
  case fdRole fd of
    Master -> void $ assertSendTo "flarei" ("node role " ++ hpId ++ " master 1 " ++ show (fdPartition fd) ++ "\r\n") "OK\r\n"
    Slave _ -> void $ assertSendTo "flarei" ("node role " ++ hpId ++ " slave 0 " ++ show (fdPartition fd) ++ "\r\n") "OK\r\n"
  void $ assertSendTo "flarei" ("node state " ++ hpId ++ " active\r\n") "OK\r\n"
  case fdRole fd of
    Slave _ -> void $ assertSendTo "flarei" ("node role " ++ hpId ++ " slave 1 " ++ show (fdPartition fd) ++ "\r\n") "OK\r\n"
    _ -> return ()

setupFlareCluster :: Sandbox ()
setupFlareCluster = withTimeout 1000 $ do
  daemons <- getVariable "daemons" [] :: Sandbox [FlareDaemon]
  mapM_ setupFlareDaemon daemons
  _ <- liftIO $ putStrLn "Waiting 2 seconds for partition table to update..."
  liftIO $ threadDelay 2000000
