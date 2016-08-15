{-# LANGUAGE DeriveGeneric #-}
module ModelDef (
    Quarter(..)
  , mapQuarters
  , mapQuartersM
) where

import Data.Aeson (ToJSON, FromJSON)
import Database.Persist.TH (derivePersistField)
import GHC.Base (Eq, Ord, mapM, Monad)
import GHC.Enum (Enum, Bounded)
import GHC.Generics (Generic)
import GHC.Read (Read)
import GHC.Show (Show)

data Quarter = First | Second | Third | Fourth | Extra
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
derivePersistField "Quarter"
instance ToJSON Quarter
instance FromJSON Quarter

mapQuarters :: (Quarter -> a) -> [a]
mapQuarters f = [f First, f Second, f Third, f Fourth, f Extra]

mapQuartersM :: Monad m => (Quarter -> m a) -> m [a]
mapQuartersM f = mapM f [First .. Extra]
