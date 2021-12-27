{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Clickhouse.Generic where

import Data.ByteString.Char8
import Data.String.Conversions
import Database.Clickhouse.Types
import GHC.Generics

class ClickRep a where
  toClickRep :: a -> [Field]
  default toClickRep :: (Generic a, GClickRep (Rep a)) => a -> [Field]
  toClickRep a = gtoClickRep (from a)

class GClickRep f where
  gtoClickRep :: f a -> [Field]

instance GClickRep U1 where
  gtoClickRep U1 = []

instance (GClickRep a, GClickRep b) => GClickRep (a :*: b) where
  gtoClickRep (a :*: b) = gtoClickRep a ++ gtoClickRep b

instance (GClickRep a, GClickRep b) => GClickRep (a :+: b) where
  gtoClickRep (L1 x) = gtoClickRep x
  gtoClickRep (R1 x) = gtoClickRep x

instance (Selector c, ToClickhouse a) => GClickRep (M1 S c (K1 i a)) where
  gtoClickRep m@(M1 x) = [(cs $ selName m, toClick (unK1 x))]

instance (GClickRep a) => GClickRep (M1 D c a) where
  gtoClickRep (M1 x) = gtoClickRep x

instance (GClickRep a) => GClickRep (M1 C c a) where
  gtoClickRep (M1 x) = gtoClickRep x
