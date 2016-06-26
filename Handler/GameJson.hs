{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module Handler.GameJson where

import Import

import           Control.Lens
import qualified Data.Aeson.Lens as AL (key, _Bool)
import qualified Data.List as L (sortBy)
import           Jabara.Persist.Util (dummyKey, toKey, toRecord)
import           Jabara.Yesod.Util (getResourcePath)

getGameIndexR :: Handler [VOGame]
getGameIndexR = runDB $ selectList [] [Asc GameDate]
  >>= mapM (\game -> entityToVo game [])

scoreUrls :: (MonadHandler m, HandlerSite m ~ App) => GameId -> m [Text]
scoreUrls gameId = do
    firstUrl <- getResourcePath $ GameScoreFirstR gameId
    secondUrl <- getResourcePath $ GameScoreSecondR gameId
    thirdUrl <- getResourcePath $ GameScoreThirdR gameId
    fourthUrl <- getResourcePath $ GameScoreFourthR gameId
    extraUrl <- getResourcePath $ GameScoreExtraR gameId
    pure [firstUrl, secondUrl, thirdUrl, fourthUrl, extraUrl]


entityToVo :: (MonadHandler m, HandlerSite m ~ App) =>
              Entity Game -> [Entity Score] -> m VOGame
entityToVo game@(Entity key _) score = do
    editPath <- getResourcePath $ GameUiR key
    path     <- getResourcePath $ GameR key
    urls     <- scoreUrls key
    pure $ VOGame {
        _voGameGame = game
      , _voGameEditUrl = editPath
      , _voGameUrl = path
      , _voGameScoreUrls = urls
      , _voGameScore = score
    }

putGameIndexR :: Handler ()
putGameIndexR = do
    req::VOGame <- requireJsonBody
    gameId <- runDB $ do
                  gameId <- insert $ toRecord $ req^.voGameGame
                  _      <- insertMany $ map (\s -> (toRecord s)&scoreGameId .~ gameId) $ req^.voGameScore
                  pure gameId
    sendResponseCreated $ GameUiR gameId

getGameR :: GameId -> Handler VOGame
getGameR gameId = do
    game     <- runDB $ core
    editPath <- getResourcePath $ GameUiR gameId
    path     <- getResourcePath $ GameR gameId
    urls     <- scoreUrls gameId
    pure $ game { _voGameEditUrl = editPath, _voGameUrl = path, _voGameScoreUrls = urls }

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
                   _voGameGame = Entity gameId game
                 , _voGameEditUrl = ""
                 , _voGameUrl = ""
                 , _voGameScoreUrls = []
                 , _voGameScore = scores
               }

postGameR :: GameId -> Handler ()
postGameR gameId = do
    req::VOGame <- requireJsonBody
    runDB $ do
        replace gameId (toRecord $ req^.voGameGame)
        mapM_ (\score -> replace (toKey score) (toRecord score)) $ req^.voGameScore

deleteGameR :: GameId -> Handler ()
deleteGameR gameId = runDB $ do
    deleteWhere [ScoreGameId ==. gameId]
    delete gameId

type QuarterIndex = Int

patchGameScoreFirstR :: GameId -> Handler ()
patchGameScoreFirstR gameId = patchGameScore gameId First

patchGameScoreSecondR :: GameId -> Handler ()
patchGameScoreSecondR gameId = patchGameScore gameId Second

patchGameScoreThirdR :: GameId -> Handler ()
patchGameScoreThirdR gameId = patchGameScore gameId Third

patchGameScoreFourthR :: GameId -> Handler ()
patchGameScoreFourthR gameId = patchGameScore gameId Fourth

patchGameScoreExtraR :: GameId -> Handler ()
patchGameScoreExtraR gameId = patchGameScore gameId Extra

patchGameScore :: GameId -> Quarter -> Handler ()
patchGameScore gameId quarter = do
    req::Value <- requireJsonBody
    (Entity key score) <- runDB $ getBy404 $ UniqueScore gameId quarter
    case req ^? AL.key "lock" . AL._Bool of
        Nothing -> pure ()
        Just (b::Bool)  -> do
                     score' <- pure $ score&scoreLock .~ b
                     _      <- runDB $ replace key score'
                     pure ()

getEmptyGameR :: Handler VOGame
getEmptyGameR = do
    now <- liftIO $ getCurrentTime
    pure $ VOGame {
      _voGameGame = Entity dummyKey $ Game {
          _gameName = ""
        , _gamePlace = ""
        , _gameTeamAName = ""
        , _gameTeamBName = ""
        , _gameDate = now
      }
    , _voGameEditUrl = ""
    , _voGameUrl = ""
    , _voGameScoreUrls = []
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
