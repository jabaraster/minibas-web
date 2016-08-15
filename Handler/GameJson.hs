{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Handler.GameJson where

import Import

import           Control.Lens (makeLenses, (^.), (^?), (.~), (&))
import qualified Data.Aeson.Lens as AL (key, _Bool, _Integer)
import           Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import qualified Data.List as L (foldl)
import           Data.Time.LocalTime (zonedTimeToUTC)
import           Jabara.Persist.Util (toKey, toRecord)
import           Jabara.Util (omittedFirstCharLower)
import           Minibas.Util (buildGameData)
import           ModelDef (Quarter(..), mapQuartersM)

getGameIndexR :: Handler [GameData]
getGameIndexR = runDB $ do
    games::[Entity Game]     <- selectList [] [Asc GameDate]
    leagues::[Entity League] <- selectList [LeagueId <-. map (_gameLeague.toRecord) games] []
    scores::[Entity Score]   <- selectList [ScoreGame <-. map toKey games] []
    teams::[Entity Team]     <- selectList [TeamId <-. (map (_gameTeamA.toRecord) games)++(map (_gameTeamB.toRecord) games)] []
    mapM (buildGameData leagues teams scores) games

data Put = Put {
    _putLeagueName :: Text
  , _putGameName :: Text
  , _putGamePlace :: Text
  , _putTeamAName :: Text
  , _putTeamBName :: Text
} deriving (Show, Eq, Read, Generic)
makeLenses ''Put
$(deriveJSON defaultOptions {
    fieldLabelModifier = omittedFirstCharLower "_put"
} ''Put)

putGameIndexR :: Handler ()
putGameIndexR = do
    put::Put <- requireJsonBody
    gameId <- runDB $ do
                leagueId <- insertIfNotExists (UniqueLeague $ put^.putLeagueName)
                                            (League $ put^.putLeagueName)
                teamAId  <- insertIfNotExists (UniqueTeam $ put^.putTeamAName)
                                            (Team $ put^.putTeamAName)
                teamBId  <- insertIfNotExists (UniqueTeam $ put^.putTeamBName)
                                            (Team $ put^.putTeamBName)
                now      <- liftIO $ getCurrentTime
                game     <- pure $ Game { _gameLeague = leagueId
                                        , _gameName = put^.putGameName
                                        , _gamePlace = put^.putGamePlace
                                        , _gameTeamA = teamAId
                                        , _gameTeamB = teamBId
                                        , _gameDate = now
                                        }
                gameId <- insert game
                _      <- mapQuartersM (\q -> insert $ emptyScore gameId q)
                pure gameId
    sendResponseCreated $ GameUiR gameId

emptyScore :: GameId -> Quarter -> Score
emptyScore gameId quarter = Score {
                              _scoreGame = gameId
                            , _scoreQuarter = quarter
                            , _scoreTeamAPoint = 0
                            , _scoreTeamBPoint = 0
                            , _scoreLock = False
                            }

insertIfNotExists :: (MonadIO m, PersistUnique (PersistEntityBackend val),
    PersistEntity val) =>
    Unique val -> val -> ReaderT (PersistEntityBackend val) m (Key val)

insertIfNotExists unique creator = do
  mEntity <- getBy unique
  case mEntity of
    Nothing -> insert creator
    Just e  -> pure $ toKey e

getGameR :: GameId -> Handler GameData
getGameR gameId = runDB $ do
    game <- get404 gameId
    leagues <- selectList [LeagueId ==. game^.gameLeague] []
    teams   <- selectList [TeamId <-. [game^.gameTeamA, game^.gameTeamB]] []
    scores  <- selectList [ScoreGame ==. gameId] []
    buildGameData leagues teams scores (Entity gameId game)

postGameR :: GameId -> Handler ()
postGameR gameId = do
    req::GameData <- requireJsonBody
    runDB $ do
        replace gameId $ toGame req
        mapM_ (\score -> replace (score^.scoreDataId) (toScore score)) $ req^.gameDataScoreList
  where
    toGame :: GameData -> Game
    toGame g = Game {
                 _gameLeague = toKey $ g^.gameDataLeague
               , _gameName = g^.gameDataName
               , _gamePlace = g^.gameDataPlace
               , _gameTeamA = toKey $ g^.gameDataTeamA
               , _gameTeamB = toKey $ g^.gameDataTeamB
               , _gameDate = zonedTimeToUTC $ g^.gameDataDate
               }
    toScore :: ScoreData -> Score
    toScore s = Score {
                  _scoreGame = gameId
                , _scoreQuarter = s^.scoreDataQuarter
                , _scoreTeamAPoint = s^.scoreDataTeamAPoint
                , _scoreTeamBPoint = s^.scoreDataTeamBPoint
                , _scoreLock = s^.scoreDataLock
                }

deleteGameR :: GameId -> Handler ()
deleteGameR gameId = runDB $ do
    deleteWhere [ScoreGame ==. gameId]
    delete gameId

type QuarterIndex = Int

patchGameScoreFirstR :: GameId -> Handler (Entity Score)
patchGameScoreFirstR gameId = patchGameScore gameId First

patchGameScoreSecondR :: GameId -> Handler (Entity Score)
patchGameScoreSecondR gameId = patchGameScore gameId Second

patchGameScoreThirdR :: GameId -> Handler (Entity Score)
patchGameScoreThirdR gameId = patchGameScore gameId Third

patchGameScoreFourthR :: GameId -> Handler (Entity Score)
patchGameScoreFourthR gameId = patchGameScore gameId Fourth

patchGameScoreExtraR :: GameId -> Handler (Entity Score)
patchGameScoreExtraR gameId = patchGameScore gameId Extra

patchGameScore :: GameId -> Quarter -> Handler (Entity Score)
patchGameScore gameId quarter = do
    req::Value <- requireJsonBody
    eScore@(Entity key score) <- runDB $ getBy404 $ UniqueScore gameId quarter
    _ <- case req of
             Object _ -> pure req
             _        -> sendResponseStatus badRequest400 eScore
    let ps::[(Score -> Score)] = [
                (\sc -> case req ^? AL.key "lock" . AL._Bool of
                            Nothing -> sc
                            Just b  -> sc&scoreLock .~ b
                )
              , (\sc -> case req ^? AL.key "teamAPoint" . AL._Integer of
                            Nothing -> sc
                            Just p  -> sc&scoreTeamAPoint .~ (fromInteger p)
                )
              , (\sc -> case req ^? AL.key "teamBPoint" . AL._Integer of
                            Nothing -> sc
                            Just p  -> sc&scoreTeamBPoint .~ (fromInteger p)
                )
              ]
        score' = L.foldl (\sc f -> f sc) score ps
    _ <- runDB $ replace key score'
    pure $ Entity key score'
