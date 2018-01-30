{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}
module Configuration where

import Data.Aeson.Types
import Data.Singletons.Prelude (SingKind(..), SingI(..))
import GHC.Generics (Generic(..))

-- The cumbersome, manual way

data State1 = State1
  { color1      :: Maybe Double
  , brightness1 :: Maybe Double
  } deriving stock (Generic)

instance ToJSON State1 where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True }

printState1 :: IO ()
printState1 = print (toJSON (State1 Nothing Nothing))

-- Alternatively...

type OmitNothingFields = Bool
newtype AesonOptions (onf :: OmitNothingFields) a = AesonOptions a

instance (Generic a, GToJSON Zero (Rep a), SingI onf)
    => ToJSON (AesonOptions onf a) where
  toJSON (AesonOptions a) =
    genericToJSON defaultOptions { omitNothingFields = fromSing (sing @_ @onf) } a

data State2 = State2
  { color2      :: Maybe Double
  , brightness2 :: Maybe Double
  } deriving stock (Generic)
    deriving ToJSON via (AesonOptions True State2)

{-
instance ToJSON State2 where
  toJSON = coerce @(AesonOptions True State2 -> Value)
                  @(State2                   -> Value)
                  toJSON
-}

printState2 :: IO ()
printState2 = print (toJSON (State2 Nothing Nothing))
