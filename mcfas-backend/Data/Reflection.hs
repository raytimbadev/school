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

type family DemoteS' (kp :: KProxy k) :: k -> *

type DemoteS (a :: k) = DemoteS' ('KProxy :: KProxy k)

type family Demote' (kp :: KProxy k) :: *

type Demote (a :: k) = Demote' ('KProxy :: KProxy k)

type instance Demote' ('KProxy :: KProxy ()) = ()
type instance Demote' ('KProxy :: KProxy Symbol) = String
type instance Demote' ('KProxy :: KProxy [k]) = [Demote' ('KProxy :: KProxy k)]
type instance Demote' ('KProxy :: KProxy Nat) = Integer

-- | Class of types that can be reflected to values.
class Reflect (a :: k) where
  reflect :: Proxy (a :: k) -> Demote a

-- | Class of types that can be reflected to singletons.
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
