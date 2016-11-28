{-|
Module      : Data.Reflection
Description : Type-level tricks for reflecting types into values
Copyright   : (c) Jacob Errington 2016
License     : MIT
Maintainer  : mcfas@mail.jerrington.me
Stability   : experimental

In Haskell, you can't pattern match on types... unless you import this module.

Two different varieties of reflection are available in this module.

 1. 'Reflect' allows you to reflect types into values that are /disconnected/
    (in some sense) from the type.
 2. 'ReflectS' allows you to reflect types into values /indexed by/ the
    original type.

In principle, style 2 allows you to do the reverse direction as well, I think.

For more information on this trick, see \"/Dependently Typed Programming with
Singletons/, published at the Haskell Symposium, 2012\".
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module Data.Reflection where

import Data.Kind
import Data.Proxy
import GHC.TypeLits hiding ( type (*) )

-- | Type-level function that converts a kind (represented as a type by a
-- proxy) into type constructor taking types from that kind to types.
type family DemoteS' (kp :: KProxy k) :: k -> *

-- | Convenient alias for 'DemoteS'' that constructs the kind proxy from an
-- explicit type.
type DemoteS (a :: k) = DemoteS' ('KProxy :: KProxy k)

-- | Type-level function that converts a kind (represented as a type by a
-- proxy) into a type.
type family Demote' (kp :: KProxy k) :: *

-- | Convenient alias for 'Demote'' that constructs the kind proxy from an
-- explicit type.
type Demote (a :: k) = Demote' ('KProxy :: KProxy k)

type instance Demote' ('KProxy :: KProxy ()) = ()
type instance Demote' ('KProxy :: KProxy Symbol) = String
type instance Demote' ('KProxy :: KProxy [k]) = [Demote' ('KProxy :: KProxy k)]
type instance Demote' ('KProxy :: KProxy Nat) = Integer

-- | Class of types that can be reflected to values.
class Reflect (a :: k) where
  reflect :: Proxy (a :: k) -> Demote a

-- | Class of types that can be reflected to indexed values.
--
-- This effectively allows you to pattern match on types.
class ReflectS (a :: k) where
  reflectS :: Proxy (a :: k) -> DemoteS a a

instance KnownNat n => Reflect (n :: Nat) where
  reflect = natVal

instance KnownSymbol s => Reflect (s :: Symbol) where
  reflect = symbolVal

instance Reflect '() where
  reflect _ = ()

instance Reflect '[] where
  reflect _ = []

instance (Reflect x, Reflect xs) => Reflect (x ': xs) where
  reflect _ = reflect x : reflect xs where
    x = Proxy :: Proxy x
    xs = Proxy :: Proxy xs
