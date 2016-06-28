{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minibas.Web (
    WVOGame(..), wvoGameGame, wvoGameUrls
  , GameUrls(..), gameUrlsGame, gameUrlsGameEdit, gameUrlsQuarter
) where

import Minibas.Types (VOGame)
import Model (Game, Score)

import ClassyPrelude.Yesod
import Control.Lens (makeLenses)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Jabara.Util (omittedFirstCharLower)
import Jabara.Yesod.Util (tc, ttc)

data GameUrls = GameUrls {
    _gameUrlsGame     :: Text
  , _gameUrlsGameEdit :: Text
  , _gameUrlsQuarter  :: [Text]
} deriving (Show, Eq, Read, Generic)
makeLenses ''GameUrls
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_gameUrls"
} ''GameUrls)

data WVOGame = WVOGame{
    _wvoGameGame :: VOGame
  , _wvoGameUrls :: GameUrls
} deriving (Show, Eq, Read, Generic)
makeLenses ''WVOGame
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_wvoGame"
} ''WVOGame)

instance ToContent WVOGame where toContent = tc
instance ToTypedContent WVOGame where toTypedContent = ttc

instance ToContent [WVOGame] where toContent = tc
instance ToTypedContent [WVOGame] where toTypedContent = ttc

instance ToContent (Entity Game) where toContent = tc
instance ToTypedContent (Entity Game) where toTypedContent = ttc

instance ToContent [Entity Game] where toContent = tc
instance ToTypedContent [Entity Game] where toTypedContent = ttc

instance ToContent (Entity Score) where toContent = tc
instance ToTypedContent (Entity Score) where toTypedContent = ttc
