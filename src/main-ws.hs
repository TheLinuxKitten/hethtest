
--------------------------------------------------------------------------
--
-- Copyright: (c) Javier López Durá
-- License: BSD3
--
-- Maintainer: Javier López Durá <linux.kitten@gmail.com>
--
--------------------------------------------------------------------------

import Control.Monad.Trans.Resource
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Network.Web3
import Network.WebSockets

instance WebSocketsData String where
  fromLazyByteString = LBS.unpack
  toLazyByteString = LBS.pack

instance WebSocketsData Value where
  fromLazyByteString = fromJust . decode
  toLazyByteString = encode

main :: IO ()
main = do
  _ <- runClient "localhost" 8545 "/" $ \con -> do
    -- enviar req
    sendTextData con "{\"id\": 1, \"method\":\"eth_subscribe\",\"params\":[\"newPendingTransactions\"]}"
    (RcpResp v r e i) <- receiveData con
    -- recibir subs
    -- unsubscribe
    sendTextData con $ RpcReq "eth_unsubscribe" [String v] (Just "1-2")
    (RcpResp v r e i) <- receiveData con
    -- close
    return ()

