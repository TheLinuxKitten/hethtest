{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception.Lifted
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Aeson.JsonRpc
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH
import Network.Web3
import Network.Web3.Dapp.EthABI
import Network.Web3.Dapp.EthABI.Bzz
import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types
import qualified Network.Web3.Dapp.Solc as Solc
import Network.Web3.Dapp.Swarm (SwarmSettings(..),defaultSwarmSettings)
import Network.Web3.Extra
import Network.Web3.HexText
import Network.Web3.Types
import System.Environment (getArgs, getEnv)

import EthDapps.Test1

enviaTx :: (JsonRpcConn c, MonadLoggerIO m, MonadBaseControl IO m)
        => HexEthAddr -> Maybe HexEthAddr -> HexData
        -> Web3T c m (Either Text RpcEthTxReceipt)
enviaTx addrFrom mAddrTo dat = web3_estimateAndSendTx addrFrom mAddrTo Nothing (Just dat)

main :: IO ()
main = do
--  test1
  testAst

getOpsAst :: IO (Bool,Bool,Bool,[Text],[FilePath],String)
getOpsAst = pAs (False,False,False,[],[],"") <$> getArgs
  where
    pAs (con,ast,json,exs,fps,url) ("--contract":url':as) = pAs (True,ast,json,exs,fps,url') as
    pAs (con,ast,json,exs,fps,url) ("--ast":as) = pAs (con,True,json,exs,fps,url) as
    pAs (con,ast,json,exs,fps,url) ("--json":as) = pAs (con,ast,True,exs,fps,url) as
    pAs (con,ast,json,exs,fps,url) ("--exclude":ex:as) = pAs (con,ast,json,T.pack ex:exs,fps,url) as
    pAs (con,ast,json,exs,fps,url) ("--sol":fp:as) = pAs (con,ast,json,exs,fp:fps,url) as
    pAs _ ("--help":as) = msgUsoAst
    pAs _ ("-h":as) = msgUsoAst
    pAs ops [] = ops
    pAs _ _ = msgUsoAst

msgUsoAst = error $ "Uso:\n"
                 ++ "    [--contract <url>]\n"
                 ++ "    [--ast]\n"
                 ++ "    [--json]\n"
                 ++ "    [--exclude <contract>]\n"
                 ++ "    --sol <file.sol>                   (puede varios)\n"

testAst :: IO ()
testAst = do
  h <- getEnv "HOME"
  (con,ast,json,exs,fps,url) <- getOpsAst
  unless (con || ast || json) $ error msgUsoAst
  ers <- Solc.compile (Solc.SolcSettings
                  [ Solc.SolcRemapping
                      Nothing
                      "github.com/modular-network/ethereum-libraries/"
                      (Just $ h ++ "/debian/haskell/hsoldapps/library/modular-network/ethereum-libraries/")
                  , Solc.SolcRemapping
                      Nothing
                      "github.com/TheLinuxKitten/ethlibs/"
                      (Just $ h ++ "/debian/haskell/hsoldapps/library/ethlibs/")
                  ] exs) fps
  when con $ mapM_ (swarmContract url) (fromRight $ Solc.fst' ers)
  when ast $ print (Solc.snd' ers)
  when json $ putStrLn $ LC8.unpack
            $ encodePretty' defConfig{confIndent=Spaces 2}
            $ fromRight $ Solc.trd' ers
  where
    swarmContract url con = do
--      print $ getBinBzzr0 $ abiContractBinRuntime con
      (addr,binCode) <- fromRight <$> (runStdoutLoggingT $ runWeb3HttpT 5 5 url $ do
        accs <- eth_accounts
        addr <- fromJust . txrContractAddress
            <$> (enviaTx (head accs) Nothing (new_in con) >>= web3FromE)
        binCode <- eth_getCode addr RPBLatest
        liftIO (uploadMetadata defaultSwarmSettings
                    (fromJust $ abiContractBin con) (abiContractMetadata con)
                  >>= print)
        return (addr,binCode))
      putStrLn "======== Download ========"
      downloadMetadata defaultSwarmSettings binCode >>= print
--      print $ decodeMetadata $ abiContractMetadata con
    new_in con = fromJust $ abiContractBin con

getOps :: FilePath -> IO (Bool,Bool,String,Maybe HexEthAddr,[FilePath])
getOps h = pAs (False,False,h++"/.ethereum/geth.ipc",Nothing,[]) <$> getArgs
  where
    pAs (log,useHttp,uri,ha,fps) ("--ipc":ipc:as) = pAs (log,False,ipc,ha,fps) as
    pAs (log,useHttp,uri,ha,fps) ("--http":url:as) = pAs (log,True,url,ha,fps) as
    pAs (log,useHttp,uri,ha,fps) ("--log":as) = pAs (True,useHttp,uri,ha,fps) as
    pAs (log,useHttp,uri,ha,fps) ("--contractAddress":addr:as) = pAs (log,useHttp,uri,Just $ HexEthAddr $ T.pack addr,fps) as
    pAs (log,useHttp,uri,ha,fps) ("--sol":fp:as) = pAs (True,useHttp,uri,ha,fp:fps) as
    pAs _ ("--help":as) = msgUso
    pAs _ ("-h":as) = msgUso
    pAs ops [] = ops
    pAs _ _ = msgUso
    msgUso = error $ "Uso:\n"
                  ++ "    [--ipc file|--http url]\n"
                  ++ "    [--log]\n"
                  ++ "    --sol <file.sol>                   (puede varios)\n"
                  ++ "    --contract nombre\n"
                  ++ "    [--contractAddress <0xaddress>]    (sino crea contrato)\n"
                  ++ "    --function nombre"

test1 :: IO ()
test1 = do
  h <- getEnv "HOME"
  (doLog,useHttp,uri,maddr,fps) <- getOps h
  let solFilePath1 = h++"/privnet/sol/ballot.sol"
  let solFilePath2 = h++"/privnet/sol/coin.sol"
  let solFilePath3 = h++"/privnet/sol/test.sol"
  let solFilePath4 = h++"/privnet/sol/types.sol"
  (ecs,_,_) <- Solc.compile (Solc.SolcSettings [] [])
                  [solFilePath1, solFilePath2, solFilePath3, solFilePath4]
  case ecs of
    Left e -> error (T.unpack e)
    Right cs -> do
      let func = head $ filter (("sam"==) . abiFuncName . abiInterfaceFunction) $ filter isInterfaceFunction $ concatMap abiContractAbi cs
      let v1 = arg3 (C8.pack "dave") True ([5,6,7,8,0,-1,-2,-3,-4,-5] :: [Int])
      let ecd = fromRight $ encodeAbi func v1
      let fs = abiFunctionSelector ecd
      let ep = abiEncodedAbiValues ecd
      let ps = takesBs 32 ep
      print $ T.pack $ show cs
--      let coinC = head $ filter (("Types"==) . abiContractName) cs
      let coinC = types_contract
      print $ T.pack $ "coinC = " ++ show coinC
      let coinC2 = $([| coinC |])
      print $ T.pack $ "coinC2 = " ++ show coinC2
      resp <- if doLog
                then runStdoutLoggingT $ if useHttp
                  then runWeb3HttpT 5 5 uri (ethAction2 coinC maddr)
                  else runWeb3IpcT 5 5 uri (ethAction2 coinC maddr)
                else runNoLoggingT $ if useHttp
                  then runWeb3HttpT 5 5 uri (ethAction2 coinC maddr)
                  else runWeb3IpcT 5 5 uri (ethAction2 coinC maddr)
      print resp
  where
    ethThreadInfo n = web3_clientVersion >>= logDebugN . (("Thread "<> T.pack (show n) <>" clientVersion: ")<>)
    ethHandle n = handle (\e -> do
      let s = "Thread " ++ show n ++ " handle: "
      logDebugN $ T.pack (s++"esperando...")
      liftIO $ threadDelay (5 * 10^6)
      logDebugN $ T.pack $ (s++) $ (show :: SomeException -> String) e )
    ethFilterThread n d bn conAddr coinC = web3Fork $ ethHandle n $ do
        logDebugN $ T.pack $ "Thread " ++ show n ++ " iniciado"
        fi <- eth_newFilter $ RpcEthFilter (Just $ RPBNum bn)
                                           (Just RPBLatest)
                                           (Just [conAddr])
                                           Nothing
        liftIO $ threadDelay (d * 10^6)
        ethThreadInfo n
        rs <- eth_getFilterChanges fi
        mapM_ (logDebugN . T.pack . (("filterChange" ++ show n ++ ": ")++)
                         . (\rfl -> case rfl of
                              EthHashFilterLog h -> show h
                              EthFilterLog fl -> show
                                               $ types_decode_log fl)) rs
        void $ eth_uninstallFilter fi
        logDebugN $ T.pack $ "Thread " ++ show n ++ " terminado"
    ethAction2 coinC maddr = do
      accs <- eth_accounts
      let addr1 = accs !! 0
      let addr2 = accs !! 1
      let addr3 = accs !! 2
      let addr4 = accs !! 3
      conAddr <- case maddr of
                  Just addr -> return addr
                  Nothing -> fromJust . txrContractAddress
                          <$> (enviaTx addr1 Nothing types_new_in >>= web3FromE)
      bn <- eth_blockNumber
      ti1 <- ethFilterThread 1 20 bn conAddr coinC
      ti2 <- ethFilterThread 2 40 bn conAddr coinC
      ti3 <- ethFilterThread 3 90 bn conAddr coinC
      etxrs <- web3_estimateAndSendTxs
                [ (addr1, Just conAddr, Nothing, Just $ types_func1_in (addr2, 200))
                , (addr1, Just conAddr, Nothing, Just $ types_func1_in (addr3, 200))
                , (addr1, Just conAddr, Nothing, Just $ types_func1_in (addr4, 200))
                , (addr1, Just conAddr, Nothing, Just $ types_func1_in (addr1, -20))
                , (addr3, Just conAddr, Nothing, Just $ types_func2_in (addr4, 20000))
                , (addr3, Just conAddr, Nothing, Just $ types_func2_in (addr1, 20))
                , (addr4, Just conAddr, Nothing, Just $ types_func2_in (addr1, -20))
                , (addr2, Just conAddr, Nothing, Just $ types_func2_in (addr1, -1))
                ]
      mapM_ (\etxr -> do
        logDebugN $ T.pack $ show etxr
        mapM (logDebugN . T.pack . show . types_decode_log)
             (txrLogs $ fromRight etxr) ) etxrs
      dat1 <- web3_call addr1 conAddr $ types_valores_in addr1
      logDebugN $ T.pack $ show addr1 ++ ": " ++ show (types_valores_out dat1)
      dat2 <- web3_call addr1 conAddr $ types_valores_in addr4
      logDebugN $ T.pack $ show addr4 ++ ": " ++ show (types_valores_out dat2)
      dat3 <- web3_call addr1 conAddr $ types_func3_in (3, False, 127)
      logDebugN $ "dat3: " <> dat3
      logDebugN $ T.pack $ "func3: " ++ show (types_func3_out dat3)
      let func4 = conFunc "func4" coinC
      dat4 <- web3_call addr1 conAddr $ types_func4_in (3, [True,False,True], -2, "locuelo", addr3, "www", True, ["uno", "dos", "tes", "cu4", "ci5"])
      logDebugN $ T.pack $ "func4: " ++ show (decodeAbi' func4 dat4)
      logDebugN $ T.pack $ "func4: " ++ show (types_func4_out dat4)
      let func5 = conFunc "func5" coinC
      dat5 <- web3_call addr1 conAddr $ types_func5_in (-1, -1)
      logDebugN $ T.pack $ "func5: " ++ show (decodeAbi' func5 dat5)
      logDebugN $ T.pack $ "func5: " ++ show (types_func5_out dat5)
      dat6 <- web3_call addr1 conAddr $ types_func5_in (127, 255)
      logDebugN $ T.pack $ "func5: " ++ show (types_func5_out dat6)
      dat7 <- web3_call addr1 conAddr $ types_func5_in (128, 256)
      logDebugN $ T.pack $ "func5: " ++ show (types_func5_out dat7)
      logDebugN $ T.pack $ "func4 -> dat7: " ++ show (decodeAbi func4 dat7 :: Either Text Types_func4_Out)
      logDebugN $ T.pack $ "func5 -> dat4: " ++ show (decodeAbi func5 dat4 :: Either Text Types_func5_Out)
      liftIO $ threadDelay (40 * 10^6)
      web3ForkWait ti2
    ethAction coinC maddr = do
      accs <- eth_accounts
      let addr1 = accs !! 0
      let addr2 = accs !! 1
      let addr3 = accs !! 2
      let addr4 = accs !! 3
      conAddr <- case maddr of
                  Just addr -> return addr
                  Nothing -> do
                    let conConstr = fromJust $ lookupInterfaceConstructor coinC
                    let conPs = abiCallDataToHexText $ fromRight
                              $ encodeAbi conConstr arg0
                    let conCode = fromJust $ abiContractBin coinC
                    fromJust . txrContractAddress <$> (enviaTx addr1 Nothing (conCode <> conPs) >>= web3FromE)
      let mintF = conFunc "func1" coinC
      let sendF = conFunc "func2" coinC
      etxrs <- web3_estimateAndSendTxs
                [ (addr1, Just conAddr, Nothing, Just $ mkData mintF (addr2, 200::Integer))
                , (addr1, Just conAddr, Nothing, Just $ mkData mintF (addr3, 200::Integer))
                , (addr1, Just conAddr, Nothing, Just $ mkData mintF (addr4, 200::Integer))
                , (addr1, Just conAddr, Nothing, Just $ mkData mintF (addr1, -20::Integer))
                , (addr3, Just conAddr, Nothing, Just $ mkData sendF (addr4, 20000::Integer))
                , (addr3, Just conAddr, Nothing, Just $ mkData sendF (addr1, 20::Integer))
                , (addr4, Just conAddr, Nothing, Just $ mkData sendF (addr1, -20::Integer))
                , (addr2, Just conAddr, Nothing, Just $ mkData sendF (addr1, -1::Integer))
                ]
      mapM_ (\etxr -> do
        logDebugN $ T.pack $ show etxr
        mapM (logDebugN . T.pack . show)
          $ decodeLogs (abiContractAbi coinC) (txrLogs $ fromRight etxr) ) etxrs
      let balancesF = conFunc "valores" coinC
      dat1 <- web3_call addr1 conAddr $ mkData balancesF (arg1 addr1)
      logDebugN $ T.pack $ show addr1 ++ ": " ++ show dat1
      dat2 <- web3_call addr1 conAddr $ mkData balancesF (arg1 addr4)
      logDebugN $ T.pack $ show addr4 ++ ": " ++ show dat2
      let func3 = conFunc "func3" coinC
      dat3 <- web3_call addr1 conAddr $ mkData func3 (3::Int, False, 127::Int)
      logDebugN $ "dat3: " <> dat3
      logDebugN $ T.pack $ "func3: " ++ show (decodeAbi func3 dat3 :: Either Text (Bool,Text,HexEthAddr))
-- func4: (uint64 a, bool[3] b, int40 c, string d, address e, bytes f, bool g, bytes3[] h)
      let func4 = conFunc "func4" coinC
      dat4 <- web3_call addr1 conAddr $ mkData func4 (3::Int, [True,False,True], -2::Int, "locuelo"::Text, addr3, "www"::BS.ByteString, True, ["uno"::BS.ByteString, "dos", "tes", "cu4", "ci5"])
--          logDebugN $ "dat4: " <> dat4
--          mapM_ (logDebugN . toHex) (takesBs 32 $ fromHex dat4)
      logDebugN $ T.pack $ "func4: " ++ show (decodeAbi' func4 dat4)
      logDebugN $ T.pack $ "func4: " ++ show (decodeAbi func4 dat4 :: Either Text (Int, [Bool], Int, Text, HexEthAddr, BS.ByteString, Bool, [BS.ByteString]))
      let func5 = conFunc "func5" coinC
      dat5 <- web3_call addr1 conAddr $ mkData func5 (-1::Int, -1::Int)
--          logDebugN $ "dat5: " <> dat5
--          mapM_ (logDebugN . toHex) (takesBs 32 $ fromHex dat5)
      logDebugN $ T.pack $ "func5: " ++ show (decodeAbi' func5 dat5)
      logDebugN $ T.pack $ "func5: " ++ show (decodeAbi func5 dat5 :: Either Text (Int, Int, Int, Int))
      dat6 <- web3_call addr1 conAddr $ mkData func5 (127::Int, 255::Int)
      logDebugN $ T.pack $ "func5: " ++ show (decodeAbi func5 dat6 :: Either Text (Int, Int, Int, Int))
      dat7 <- web3_call addr1 conAddr $ mkData func5 (128::Int, 256::Int)
      logDebugN $ T.pack $ "func5: " ++ show (decodeAbi func5 dat7 :: Either Text (Int, Int, Int, Int))
      logDebugN $ T.pack $ "func4 -> dat7: " ++ show (decodeAbi func4 dat7 :: Either Text (Int, [Bool], Int, Text, HexEthAddr, BS.ByteString, Bool, [BS.ByteString]))
      logDebugN $ T.pack $ "func5 -> dat4: " ++ show (decodeAbi func5 dat4 :: Either Text (Int, Int, Int, Int))
    conFunc f = fromJust . lookupInterfaceFunction f
    mkData :: AbiValueEncoding a => Interface -> a -> HexData
    mkData f = joinHex . abiCallDataToHexText . fromRight . encodeAbi f

takesBs = takesBs' []
  where
    takesBs' r n bs =
      if BS.null bs
        then reverse r
        else let (l,bs') = BS.splitAt n bs in takesBs' (l:r) n bs'

