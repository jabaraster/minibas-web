{-# LANGUAGE DeriveGeneric #-}

module Minibas.Types (
    VOGame(..), voGameName, voGamePlace, voGameScores
) where

import Model (Score)

import Control.Lens (makeLenses)
import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Database.Persist (Entity)
import GHC.Base (Eq)
import GHC.Generics (Generic)
import GHC.Read (Read)
import GHC.Show (Show)
import Jabara.Util (omittedFirstCharLower)
import Jabara.Yesod.Util (tc, ttc)
import Yesod.Core.Content (ToContent(..), ToTypedContent(..))

data VOGame = VOGame {
    _voGameName :: Text
  , _voGamePlace :: Text
  , _voGameScores :: [Entity Score]
} deriving (Show, Eq, Read, Generic)
makeLenses ''VOGame
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_voGame"
} ''VOGame)
instance ToContent VOGame where toContent = tc
instance ToTypedContent VOGame where toTypedContent = ttc
