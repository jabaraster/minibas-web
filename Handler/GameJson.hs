{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Handler.GameJson where

import Import

import           Control.Lens
import qualified Data.Aeson.Lens as AL (key, _Bool, _Integer)
import qualified Data.List as L (sortBy, foldl)
import           Jabara.Persist.Util (dummyKey, toKey, toRecord)
import           Jabara.Yesod.Util (getResourcePath)

getGameIndexR :: Handler [WVOGame]
getGameIndexR = runDB $ selectList [] [Asc GameDate]
  >>= mapM entityToVo

quarterUrls :: (MonadHandler m, HandlerSite m ~ App) => GameId -> m [Text]
quarterUrls gameId = do
    firstUrl  <- getResourcePath $ GameScoreFirstR gameId
    secondUrl <- getResourcePath $ GameScoreSecondR gameId
    thirdUrl  <- getResourcePath $ GameScoreThirdR gameId
    fourthUrl <- getResourcePath $ GameScoreFourthR gameId
    extraUrl  <- getResourcePath $ GameScoreExtraR gameId
    pure [firstUrl, secondUrl, thirdUrl, fourthUrl, extraUrl]


entityToVo :: (MonadHandler m, HandlerSite m ~ App) => Entity Game -> m WVOGame
entityToVo game@(Entity key _) = do
    editPath <- getResourcePath $ GameUiR key
    path     <- getResourcePath $ GameR key
    urls     <- quarterUrls key
    pure WVOGame {
        _wvoGameGame = VOGame {
            _voGameProperty = game
          , _voGameScore = []
        }
      , _wvoGameUrls = GameUrls {
            _gameUrlsGame = path
          , _gameUrlsGameEdit = editPath
          , _gameUrlsQuarter = urls
        }
    }

putGameIndexR :: Handler ()
putGameIndexR = do
    req::VOGame <- requireJsonBody
    gameId <- runDB $ do
                  gameId <- insert $ toRecord $ req^.voGameProperty
                  _      <- insertMany $ map (\s -> (toRecord s)&scoreGameId .~ gameId) $ req^.voGameScore
                  pure gameId
    sendResponseCreated $ GameUiR gameId

getGameR :: GameId -> Handler WVOGame
getGameR gameId = do
    game     <- runDB $ core
    editPath <- getResourcePath $ GameUiR gameId
    path     <- getResourcePath $ GameR gameId
    urls     <- quarterUrls gameId
    pure WVOGame {
             _wvoGameGame = game
           , _wvoGameUrls = GameUrls {
                 _gameUrlsGame = path
               , _gameUrlsGameEdit = editPath
               , _gameUrlsQuarter = urls
             }
         }

  where
    core :: MonadIO m => ReaderT SqlBackend m VOGame
    core = do
        game <- get404 gameId
        scores   <- selectList [ScoreGameId ==. gameId] []
                    >>= pure . L.sortBy (
                            \r l -> let r' = (toRecord r)^.scoreQuarter
                                        l' = (toRecord l)^.scoreQuarter
                                    in  compare r' l'
                        )
        pure $ VOGame {
                   _voGameProperty = Entity gameId game
                 , _voGameScore = scores
               }

postGameR :: GameId -> Handler ()
postGameR gameId = do
    req::VOGame <- requireJsonBody
    runDB $ do
        replace gameId (toRecord $ req^.voGameProperty)
        mapM_ (\score -> replace (toKey score) (toRecord score)) $ req^.voGameScore

deleteGameR :: GameId -> Handler ()
deleteGameR gameId = runDB $ do
    deleteWhere [ScoreGameId ==. gameId]
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

getEmptyGameR :: Handler WVOGame
getEmptyGameR = do
    now <- liftIO $ getCurrentTime
    pure $ WVOGame {
        _wvoGameUrls = GameUrls {
                           _gameUrlsGame = ""
                         , _gameUrlsGameEdit = ""
                         , _gameUrlsQuarter = []
                       }
      , _wvoGameGame = VOGame {
          _voGameProperty = Entity dummyKey $ Game {
                                _gameName = ""
                              , _gamePlace = ""
                              , _gameTeamAName = ""
                              , _gameTeamBName = ""
                              , _gameDate = now
                            }
        , _voGameScore = [
              Entity dummyKey $ Score {
                  _scoreGameId = dummyKey
                , _scoreQuarter = First
                , _scoreTeamAPoint  = 0
                , _scoreTeamBPoint  = 0
                , _scoreLock = False
              }
            , Entity dummyKey $ Score {
                  _scoreGameId = dummyKey
                , _scoreQuarter = Second
                , _scoreTeamAPoint  = 0
                , _scoreTeamBPoint  = 0
                , _scoreLock = False
              }
            , Entity dummyKey $ Score {
                  _scoreGameId = dummyKey
                , _scoreQuarter = Third
                , _scoreTeamAPoint  = 0
                , _scoreTeamBPoint  = 0
                , _scoreLock = False
              }
            , Entity dummyKey $ Score {
                  _scoreGameId = dummyKey
                , _scoreQuarter = Fourth
                , _scoreTeamAPoint  = 0
                , _scoreTeamBPoint  = 0
                , _scoreLock = False
              }
            , Entity dummyKey $ Score {
                  _scoreGameId = dummyKey
                , _scoreQuarter = Extra
                , _scoreTeamAPoint  = 0
                , _scoreTeamBPoint  = 0
                , _scoreLock = False
              }
          ]
      }
  }
