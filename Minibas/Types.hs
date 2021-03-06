{-# LANGUAGE DeriveGeneric #-}

module Minibas.Types (
    VOGame(..), voGameProperty, voGameScore
) where

import ClassyPrelude.Yesod
import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Jabara.Util (omittedFirstCharLower)
import Jabara.Yesod.Util (tc, ttc)
import Model (Score, Game)

data VOGame = VOGame {
    _voGameProperty :: Entity Game
  , _voGameLeagueName :: Text
  , _voGameScore    :: [Entity Score]
} deriving (Show, Eq, Read, Generic)
makeLenses ''VOGame
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_voGame"
} ''VOGame)

instance ToContent VOGame where toContent = tc
instance ToTypedContent VOGame where toTypedContent = ttc

instance ToContent [VOGame] where toContent = tc
instance ToTypedContent [VOGame] where toTypedContent = ttc
