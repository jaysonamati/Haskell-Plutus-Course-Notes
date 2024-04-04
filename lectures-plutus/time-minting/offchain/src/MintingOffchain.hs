{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MintingOffchain where

import           GeniusYield.TxBuilder (GYTxMonad, GYTxSkeleton, mustHaveOutput, GYTxQueryMonad, scriptAddress, runGYTxMonadNode, ownAddresses, mustMint, mustBeSignedBy, addressToPubKeyHash')
import           GeniusYield.Types (GYMintingPolicy, GYMintScript (..), GYValue, GYTime, GYPubKeyHash, GYTxOut (..), PlutusVersion (..), mintingPolicyFromPlutus, redeemerFromPlutusData, timeToPlutus, pubKeyHashToPlutus, datumFromPlutusData, GYTxOutUseInlineDatum (..), GYValidator, GYAddress, validatorFromPlutus, GYTxId, readPaymentSigningKey, paymentVerificationKey, pubKeyHash, addressFromPubKeyHash, signGYTxBody, GYProviders (gySubmitTx))
import qualified Minting
import GeniusYield.GYConfig (coreConfigIO, GYCoreConfig (cfgNetworkId), withCfgProviders)


mkMintingPolicy :: GYPubKeyHash -> GYMintingPolicy 'PlutusV2
mkMintingPolicy pkh = mintingPolicyFromPlutus $ Minting.mkMintingPolicy $ pubKeyHashToPlutus pkh

{-
mintingPolicy :: GYMintingPolicy 'PlutusV2
mintingPolicy = mintingPolicyFromPlutus Minting.mintingPolicy
-}

mint :: GYTxMonad m => Integer -> m (GYTxSkeleton 'PlutusV2)
mint amount = do
    addr <- head <$> ownAddresses
    pkh  <- addressToPubKeyHash' addr
    pure $ 
        mustMint (GYMintScript (mkMintingPolicy pkh)) (redeemerFromPlutusData ()) "Kenya" amount <>
        mustBeSignedBy pkh


mintIO :: FilePath -> FilePath -> Integer -> IO GYTxId
mintIO cfgFile skeyFile amount = do
    cfg <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey = paymentVerificationKey skey
        pkh  = pubKeyHash vkey
        nid  = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Minting" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (mint amount)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx