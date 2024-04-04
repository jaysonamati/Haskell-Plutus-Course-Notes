module Offchain where

import GeniusYield.Types
import GeniusYield.TxBuilder
import qualified Gift

giftValidator :: GYValidator PlutusV2
giftValidator = validatorFromPlutus Gift.giftValidator

giftValidatorAddress :: GYTxQueryMonad m => m GYAddress
giftAddress = scriptAddress giftValidator

makeGift :: GYTxMonad m => GYValue -> m (GYTxSkeleton PlutusV2)
makeGift v = do
    addr <- giftAddress
    mustHaveOutput GYTxOut
        { gyTxOutAddress = addr
        , gyTxOutValue   = v
        , gyTxOutDatum   = Just (datumFromPlutusData (), GYTxOutUseInlineDatum)
        , gyTxOutRefS    = Nothing
        }

collectGift :: GYTxMonad => GYTxOutRef -> m (GYTxSkeleton 'PlutusV2)
collectGift ref = do
    let script = validatorToScript validator
        datum  = datumFromPlutusData ()
    pure $ mustHaveInput GYTxIn
        { gyTxInTxOutRef = ref
        , gyTxInWitness  = GYTxInWitnessScript (GYInScript PlutusV2 script) datum redeemer
        }

makeGiftIO :: FilePath -> FilePath -> GYValue -> IO GYTxId
makeGiftIO cfgFile skeyFile v = do
    cfg <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey  = paymentVerification skey
        pkh   = pubKeyHash vkey
        nid   = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Gift" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (makeGift v)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx


makeGiftIO :: FilePath -> FilePath -> GYTxOutRef -> IO GYTxId
makeGiftIO cfgFile skeyFile ref = do
    cfg <- coreConfigIO cfgFile
    skey <- readPaymentSigningKey skeyFile
    let vkey  = paymentVerification skey
        pkh   = pubKeyHash vkey
        nid   = cfgNetworkId cfg
        addr = addressFromPubKeyHash nid pkh
    withCfgProviders cfg "Gift" $ \providers -> do
        txBody <- runGYTxMonadNode
            nid
            providers
            [addr]
            addr
            Nothing
            (collectGift ref)
        let tx = signGYTxBody txBody [skey]
        gySubmitTx providers tx

