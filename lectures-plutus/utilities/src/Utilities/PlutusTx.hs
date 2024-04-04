{-# LANGUAGE NoImplicitPrelude #-}

module Utilities.PlutusTx
  ( wrapValidator
  , wrapPolicy
  , wrapParameterizedPolicy
  , wrapStakeValidator
  ) where

import           PlutusLedgerApi.V2 (ScriptContext, UnsafeFromData,
                                       unsafeFromBuiltinData)
import           PlutusTx.Prelude     (Bool, BuiltinData, check, ($))

{-# INLINABLE wrapValidator #-}
wrapValidator :: ( UnsafeFromData a
                 , UnsafeFromData b
                 )
              => (a -> b -> ScriptContext -> Bool)
              -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapValidator f a b ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

{-# INLINABLE wrapPolicy #-}
wrapPolicy :: UnsafeFromData a
           => (a -> ScriptContext -> Bool)
           -> (BuiltinData -> BuiltinData -> ())
wrapPolicy f a ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData ctx)

{-# INLINABLE wrapParameterizedPolicy #-}
wrapParameterizedPolicy :: (UnsafeFromData a, UnsafeFromData b)
           => (a -> b -> ScriptContext -> Bool)
           -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapParameterizedPolicy f a b ctx =
  check $ f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

{-# INLINABLE wrapStakeValidator #-}
wrapStakeValidator :: UnsafeFromData a
                     => (a -> ScriptContext -> Bool)
                     -> (BuiltinData -> BuiltinData -> ())
wrapStakeValidator = wrapPolicy