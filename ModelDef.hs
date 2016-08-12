{-# LANGUAGE DeriveGeneric #-}
module ModelDef (
    Quarter(..)
) where

import Data.Aeson (ToJSON, FromJSON)
import Database.Persist.TH (derivePersistField)
import GHC.Base (Eq, Ord)
import GHC.Enum (Enum, Bounded)
import GHC.Generics (Generic)
import GHC.Read (Read)
import GHC.Show (Show)

data Quarter = First | Second | Third | Fourth | Extra
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "Quarter"
instance ToJSON Quarter
instance FromJSON Quarter
