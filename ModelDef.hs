{-# LANGUAGE DeriveGeneric #-}

module ModelDef (
    Quarter(..)
) where

import ClassyPrelude.Yesod

data Quarter = First | Second | Third | Fourth | Extension
    deriving (Show, Read, Eq, Enum, Bounded, Generic)
derivePersistField "Quarter"
instance ToJSON Quarter
instance FromJSON Quarter
