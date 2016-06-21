{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model (
    module Model
  , module ModelDef
) where

import Data.ByteString
import Data.Text
import Data.Time.Clock
import Data.Typeable (Typeable)
import Database.Persist.TH (share, mkPersist, sqlSettings
                           , mkMigrate, persistFileWith
                           )

import Database.Persist.Quasi (lowerCaseSettings)

import GHC.Base
import Prelude

import Data.Aeson
import Database.Persist
import GHC.Generics
import ModelDef

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON Score
instance ToJSON (Entity Score)
