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

module EthDapps.Test1 where

import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types

$(compile (SolcSettings [] [])
    [ "/home/kitten/privnet/sol/coin.sol"
    , "/home/kitten/privnet/sol/types.sol"
    ])


