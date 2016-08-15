module Minibas.Util (
    totalScore
  , quarterUrls
  , buildScoreData
  , buildGameData
) where

import Import

import           Control.Lens ((^.))
import qualified Data.List as L (foldl, sortOn, (!!))
import qualified Data.Map as M (Map, lookup)
import           Data.Maybe (fromJust)
import           Jabara.Persist.Util (toRecord)
import           Jabara.Yesod.Util (getResourcePath)

totalScore :: [Entity Score] -> (Int, Int)
totalScore = L.foldl (\(a,b) (Entity _ score) ->
    let as = score^.scoreTeamAPoint
        bs = score^.scoreTeamBPoint
    in  (a+as,b+bs)
                   ) (0,0)

quarterUrls :: (MonadHandler m, HandlerSite m ~ App) => GameId -> m [Text]
quarterUrls gameId = do
    firstUrl  <- getResourcePath $ GameScoreFirstR gameId
    secondUrl <- getResourcePath $ GameScoreSecondR gameId
    thirdUrl  <- getResourcePath $ GameScoreThirdR gameId
    fourthUrl <- getResourcePath $ GameScoreFourthR gameId
    extraUrl  <- getResourcePath $ GameScoreExtraR gameId
    pure [firstUrl, secondUrl, thirdUrl, fourthUrl, extraUrl]

buildScoreData :: (MonadHandler m, HandlerSite m ~ App) =>
                    GameId -> Entity Score -> m ScoreData
buildScoreData gameId (Entity key score) = do
    urls <- quarterUrls gameId
    url  <- do
              let idx = fromEnum $ score^.scoreQuarter
              pure (urls L.!! idx)
    pure $ ScoreData {
             _scoreDataId = key
           , _scoreDataGame = gameId
           , _scoreDataQuarter = score^.scoreQuarter
           , _scoreDataTeamAPoint = score^.scoreTeamAPoint
           , _scoreDataTeamBPoint = score^.scoreTeamBPoint
           , _scoreDataLock = score^.scoreLock
           , _scoreDataUrlBase = url
           }

buildGameData :: (MonadHandler m, HandlerSite m ~ App) =>
                   M.Map LeagueId League
                   -> M.Map TeamId Team
                   -> M.Map GameId [Entity Score]
                   -> Entity Game
                   -> m GameData
buildGameData leagueMap teamMap scoreMap (Entity gameId game) = do
      urlBase <- getResourcePath $ GameR gameId
      urlEdit <- getResourcePath $ GameUiR gameId
      let scores  = L.sortOn (_scoreQuarter.toRecord)
                       $ fromJust $ M.lookup gameId scoreMap
          total   = totalScore scores
          league  = getFromMap (game^.gameLeague) leagueMap
          teamA   = getFromMap (game^.gameTeamA) teamMap
          teamB   = getFromMap (game^.gameTeamB) teamMap
      scores' <- mapM (buildScoreData gameId) scores
      pure $ GameData {
               _gameDataId = gameId
             , _gameDataName = game^.gameName
             , _gameDataLeague = league
             , _gameDataLeagueName = (toRecord league)^.leagueName
             , _gameDataTeamA = teamA
             , _gameDataTeamAName = (toRecord teamA)^.teamName
             , _gameDataTeamBName = (toRecord teamB)^.teamName
             , _gameDataTeamB = teamB
             , _gameDataTeamAScore = fst total
             , _gameDataTeamBScore = snd total
             , _gameDataUrlBase = urlBase
             , _gameDataUrlEdit = urlEdit
             , _gameDataScoreList = scores'
             }
  where
    getFromMap :: PersistEntity r => Key r -> Map (Key r) r -> Entity r
    getFromMap key entityMap = Entity key $ fromJust $ M.lookup key entityMap
