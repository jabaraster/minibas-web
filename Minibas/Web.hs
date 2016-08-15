{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minibas.Web (
    WVOGame(..), wvoGameGame, wvoGameUrls
  , GameUrls(..), gameUrlsGame, gameUrlsGameEdit, gameUrlsQuarter
  , ScoreData(..), scoreDataId, scoreDataGame, scoreDataQuarter, scoreDataTeamAPoint, scoreDataTeamBPoint, scoreDataLock, scoreDataUrlBase
  , GameData(..), gameDataId, gameDataName, gameDataLeague, gameDataLeagueName, gameDataTeamA, gameDataTeamAName, gameDataTeamB, gameDataTeamBName, gameDataTeamAScore, gameDataTeamBScore, gameDataUrlBase, gameDataUrlEdit, gameDataScoreList
) where

import Minibas.Types (VOGame)
import Model
import ModelDef (Quarter)

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

data ScoreData = ScoreData {
    _scoreDataId :: ScoreId
  , _scoreDataGame :: GameId
  , _scoreDataQuarter :: Quarter
  , _scoreDataTeamAPoint :: Int
  , _scoreDataTeamBPoint :: Int
  , _scoreDataLock :: Bool
  , _scoreDataUrlBase :: Text
} deriving (Show, Eq, Read, Generic)
makeLenses ''ScoreData
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_scoreData"
} ''ScoreData)

data GameData = GameData {
    _gameDataId :: GameId
  , _gameDataName :: Text
  , _gameDataLeague :: Entity League
  , _gameDataLeagueName :: Text
  , _gameDataTeamA :: Entity Team
  , _gameDataTeamAName :: Text
  , _gameDataTeamB :: Entity Team
  , _gameDataTeamBName :: Text
  , _gameDataTeamAScore :: Int
  , _gameDataTeamBScore :: Int
  , _gameDataUrlBase :: Text
  , _gameDataUrlEdit :: Text
  , _gameDataScoreList :: [ScoreData]
} deriving (Show, Eq, Read, Generic)
makeLenses ''GameData
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_gameData"
} ''GameData)
instance ToContent [GameData] where toContent = tc
instance ToTypedContent [GameData] where toTypedContent = ttc

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

instance ToContent [Entity League] where toContent = tc
instance ToTypedContent [Entity League] where toTypedContent = ttc

instance ToContent [Entity Team] where toContent = tc
instance ToTypedContent [Entity Team] where toTypedContent = ttc
