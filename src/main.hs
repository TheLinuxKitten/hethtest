{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Main where

import Data.Aeson.JsonRpc
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Data.Maybe
import Data.Monoid
import Network.JsonRpcCliHttp
import Network.JsonRpcConn
import Network.JsonRpcSrvHttp
import Network.Web3
import Network.Web3.Types
import System.Environment (getArgs,getProgName)

foldBlockchain :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
               => (Int64 -> Web3T c m ())
               -> Web3T c m ()
foldBlockchain = foldFrom 0
  where
    foldFrom desde f = do
      numBlks <- eth_blockNumber
      when (desde <= numBlks) $ do
          let lst = replicate (fromIntegral $ numBlks - desde + 1) ()
          foldM_ (\blkN _ -> f blkN >> return (blkN + 1)) desde lst
          foldFrom (numBlks + 1) f

printBlockchain :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
                => Web3T c m ()
printBlockchain = foldBlockchain $ \blkN -> do
  blk <- eth_getBlock (PBNum blkN) True
  liftIO $ do
    putStrLn $ "Bloque " ++ show blkN
    print blk
  liftIO $ putStrLn "Traces"
  mapM_ (\tx -> debug_traceTransaction (btxHash $ fromPOObject tx) defaultTraceOptions
                    >>= liftIO . print) $ rebTransactions $ fromJust blk
  liftIO $ putStrLn "Dump state"
  debug_dumpBlock (fromJust $ rebNumber $ fromJust blk) >>= liftIO . print
  where
    fromPOObject (POObject a) = a

rpcM :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m) => Web3T c m ()
rpcM = do
  let blkHash1 = "0x000000000000000000000000000000000000000000000001aabbbbbbbbbbbbbb"
  let txHash1 = "0x000000000000000000000000000000000000000000000001aabbbbbbbbbbbbbb"
  web3_clientVersion >>= pT
  web3_sha3 "0xabc0" >>= pT
  net_version >>= pT
  net_listening >>= pO
  net_peerCount >>= pO
  eth_protocolVersion >>= pT
  eth_syncing >>= pO
  eth_coinbase >>= pO
  eth_mining >>= pO
  eth_hashrate >>= pO
  eth_gasPrice >>= pO
  acs <- eth_accounts
  let addr1 = if length acs >= 3 then acs!!2 else head acs
  let addr2 = if length acs >= 2 then acs!!1 else head acs
  mapM_ pO acs
  eth_blockNumber >>= pO
  eth_getBalance addr1 RPBLatest >>= pO
  mapM (`eth_getBalance` RPBLatest) acs >>= mapM_ pO
  eth_getStorageAt addr1 0 RPBLatest >>= pT
  eth_getTransactionCount addr1 RPBLatest >>= pO
  eth_getTransactionCount addr1 (RPBNum 12) >>= pO
  --eth_getBlockTransactionCountByHash blkHash1 >>= pO
  eth_getBlockTransactionCountByNumber (RPBNum 8) >>= pO
  --eth_getUncleCountByBlockHash blkHash1 >>= pO
  eth_getUncleCountByBlockNumber (RPBNum 8) >>= pO
  eth_getCode addr1 RPBLatest >>= pT
  {-eth_sign addr2 "0xaabbddeeff00223388554477116699cc" >>= pT-}
  --eth_getBlockByHash blkHash1 True >>= pO
  eth_getBlock (PBNum 0) True >>= pO
  eth_getBlock (PBNum 1) False >>= pO
  eth_getBlock (PBNum 1) True >>= pO
  eth_getBlock (PBNum 14) True >>= pO
  eth_getBlock PBLatest True >>= pO
  eth_getBlock PBPending True >>= pO
  --eth_getTransactionByHash txHash1 >>= pO
  --eth_getTransactionByBlockHashAndIndex txHash1 "0x0" >>= pO
  --eth_getTransactionByBlockAndIndex (PBNum "0xdea3") "0x0" >>= pO
  eth_getTransactionByBlockAndIndex PBLatest 0 >>= pO
  --eth_getTransactionReceipt txHash1 >>= pO
  --eth_getUncleByBlockHashAndIndex txHash1 "0x0" >>= pO
  --eth_getUncleByBlockNumberAndIndex (PBNum "0x1b4") "0x0" >>= pO
  --eth_getUncleByBlockNumberAndIndex PBLatest "0x0" >>= pO
  --eth_getCompilers >>= mapM_ pT
  --eth_compileSolidity "contract test { function multiply(uint a) return(uint d) { return a * 7; } }" >>= pO
  --eth_compileLLL "(returnlll (suicide (caller)))" >>= pT
  --eth_compileSerpent "/* codigo serpent */" >>= pT
  eth_coinbase >>= flip eth_getBalance RPBLatest >>= pO
  eth_getBlock (PBNum 69) False
    >>= mapM (flip debug_traceTransaction defaultTraceOptions . fromPOHash)
            . rebTransactions . fromJust >>= pO
  debug_dumpBlock 69 >>= pO
  where
    fromPOHash (POHash h) = h
    pT = liftIO . putStrLn . T.unpack
    pO :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m, Show a) => a -> Web3T c m ()
    pO = liftIO . print

main :: IO ()
main = do
  as <- getArgs
  --runNoLoggingT (runWeb3IpcT web3_clientVersion) >>= putStrLn . (show :: Either Text Text -> String)
  case as of
    ["-h"] -> showMsgUso
    ["--help"] -> showMsgUso
    ["--http",port] -> runJsonRpcHttpServer (read port) fSrv
    ["--https",port,cert,key] -> runJsonRpcHttpsServer (read port) cert key fSrv
    ["--fcgi",nThreads] -> runJsonRpcFastCGIServer (read nThreads) fSrv
    ["--client",url] -> do
      let rq = parseRequest_ url
      void $ runStdoutLoggingT $ runJsonRpcHttpT rq $ do
        sendJsonRpc JsonRpcV2 SrvReq1 >>= pLn
        sendJsonRpc JsonRpcV2 (SrvReq2 False) >>= pLn
        sendJsonRpc JsonRpcV2 (SrvReq3 "Parametro 1" "Astro") >>= pLn
        sendJsonRpc JsonRpcV2 SrvReq4 >>= pLn
    ["--web3-http",url] -> mapM_ (\f -> runStdoutLoggingT (runWeb3HttpT 5 5 url f) >>= print) [printBlockchain]
    ["--web3-http-test",url] -> mapM_ (\f -> runStdoutLoggingT (runWeb3HttpT 5 5 url f) >>= print) [rpcM]
    ["--web3-ipc",ipc] -> mapM_ (\f -> runStdoutLoggingT (runWeb3IpcT 5 5 ipc f) >>= print) [printBlockchain]
    ["--web3-ipc-test",ipc] -> mapM_ (\f -> runStdoutLoggingT (runWeb3IpcT 5 5 ipc f) >>= print) [rpcM]
    _ -> showMsgUso
  where
    fSrv :: SrvReq -> IO (JsonRpcResp Value)
    fSrv req = return $ Right $ toJSON $ show req
    showMsgUso = do
      exe <- getProgName
      putStrLn $ exe <> " {-h|--help}"
      putStrLn $ exe <> " --http <port>"
      putStrLn $ exe <> " --https <port> <crt> <key>"
      putStrLn $ exe <> " --fcgi <max-threads>"
      putStrLn $ exe <> " --client <url>"
      putStrLn $ exe <> " --web3-http <url>"
      putStrLn $ exe <> " --web3-http-test <url>"
      putStrLn $ exe <> " --web3-ipc <geth.ipc>"
      putStrLn $ exe <> " --web3-ipc-test <geth.ipc>"
    pLn = liftIO . putStrLn
    url1 = "http://localhost:12333/cgi-bin/hethrpc"
    url2 = "https://localhost:12334/cgi-bin/hethrpc"
    url3 = "http://localhost:12335/cgi-bin/hethrpc"
    url4 = "http://192.168.122.16:8545"

data SrvReq = SrvReq1
            | SrvReq2 Bool
            | SrvReq3 Text Text
            | SrvReq4
            deriving (Show)

instance FromJsonRpcRequest SrvReq where
  fromJsonRpcRequest' :: JsonRpcMethod -> [Value] -> JsonRpcResp SrvReq
  fromJsonRpcRequest' "m1" [] = Right SrvReq1
  fromJsonRpcRequest' "m2" [Bool b] = Right $ SrvReq2 b
  fromJsonRpcRequest' "m3" [String t1, String t2] = Right $ SrvReq3 t1 t2
  fromJsonRpcRequest' "m4" [] = Right SrvReq4
  fromJsonRpcRequest' m v = Left $ "fromJsonRpcRequest': " <> m <> " " <> T.pack (show v)

instance ToRequest SrvReq where
  requestMethod SrvReq1 = "m1"
  requestMethod (SrvReq2 _) = "m2"
  requestMethod (SrvReq3 _ _) = "m3"
  requestMethod SrvReq4 = "m4"
  requestIsNotif _ = False

instance ToJSON SrvReq where
  toJSON SrvReq1 = toArrayValue []
  toJSON (SrvReq2 b) = toArrayValue [Bool b]
  toJSON (SrvReq3 t1 t2) = toArrayValue [String t1, String t2]
  toJSON SrvReq4 = toArrayValue []

