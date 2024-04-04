{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Gift where

import PlutusTx  (BuiltinData, CompiledCode, compile)
import Prelude   (IO)
import Utilities (writeCodeToFile)

mkGiftValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkGiftValidator _datum _redeemer _ctx = ()
{-# INLINABLE mkGiftValidator #-} -- this means the compiler will make the definition inlinable

giftValidator :: CompileCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
giftValidator = $$(PlutusTx.compile [|| mkGiftValidator ||])

saveVal :: IO ()
saveVal = writeCodeToFile "../assets/gift.plutus" giftValidator

saveDatum :: IO ()
saveDatum = writeDataToFile "../assets/unit-datum.json" ()