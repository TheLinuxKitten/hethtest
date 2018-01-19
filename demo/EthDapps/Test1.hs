{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module EthDapps.Test1 where

import Network.Web3.Dapp.EthABI.TH
import Network.Web3.Dapp.EthABI.Types

$(compile (SolcSettings [] [])
    [ "/home/kitten/privnet/sol/coin.sol"
    , "/home/kitten/privnet/sol/types.sol"
    ])


