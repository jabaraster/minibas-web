{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GameJson where

import Import

import           Control.Lens
import qualified Data.List as L (sortBy)
import           Jabara.Persist.Util (dummyKey, toRecord)
import           Jabara.Yesod.Util (getResourcePath)

getGameIndexR :: Handler [VOGame]
getGameIndexR = runDB $ selectList [] [Asc GameDate]
  >>= mapM (\game -> entityToVo game [])

entityToVo :: (MonadHandler m, HandlerSite m ~ App) =>
              Entity Game -> [Entity Score] -> m VOGame
entityToVo game@(Entity key _) score = do
    path <- getResourcePath $ GameUiR key
    pure $ VOGame {
        _voGameGame = game
      , _voGameEditUrl = path
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
    game <- runDB $ core
    path <- getResourcePath $ GameUiR gameId
    pure $ game&voGameEditUrl .~ path

  where
    core :: MonadIO m => ReaderT SqlBackend m VOGame
    core = do
        game <- get404 gameId
        scores <- selectList [ScoreGameId ==. gameId] []
                    >>= pure . L.sortBy (
                            \r l -> let r' = (toRecord r)^.scoreQuarter
                                        l' = (toRecord l)^.scoreQuarter
                                    in  compare r' l'
                        )
        pure $ VOGame {
                   _voGameGame = Entity gameId game
                 , _voGameEditUrl = ""
                 , _voGameScore = scores
               }

postGameR :: GameId -> Handler ()
postGameR gameId = undefined

deleteGameR :: GameId -> Handler ()
deleteGameR gameId = runDB $ do
    deleteWhere [ScoreGameId ==. gameId]
    delete gameId

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
    , _voGameScore = [
          Entity dummyKey $ Score {
              _scoreGameId = dummyKey
            , _scoreQuarter = First
            , _scoreTeamAPoint  = 0
            , _scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              _scoreGameId = dummyKey
            , _scoreQuarter = Second
            , _scoreTeamAPoint  = 0
            , _scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              _scoreGameId = dummyKey
            , _scoreQuarter = Third
            , _scoreTeamAPoint  = 0
            , _scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              _scoreGameId = dummyKey
            , _scoreQuarter = Fourth
            , _scoreTeamAPoint  = 0
            , _scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              _scoreGameId = dummyKey
            , _scoreQuarter = Extension
            , _scoreTeamAPoint  = 0
            , _scoreTeamBPoint  = 0
          }
      ]
  }
