{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GameJson where

import Import

import           Control.Lens
import qualified Data.List as L (sortBy)
import           Jabara.Persist.Util (dummyKey, toRecord, toKey)
import           Jabara.Yesod.Util (getResourcePath)

getGameIndexR :: Handler [VOGame]
getGameIndexR = runDB $ selectList [] [Asc GameDate]
  >>= mapM (\game -> entityToVo game [])

-- entityToVo :: Entity Game -> [Entity Score] -> Handler VOGame
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
                  _      <- mapM (\s -> insert (toRecord s) { scoreGameId = gameId }) $ req^.voGameScore
                  pure gameId
    sendResponseCreated $ GameUiR gameId

getGameR :: GameId -> Handler VOGame
getGameR gameId = do
    game <- runDB $ core gameId
    path <- getResourcePath $ GameUiR gameId
    pure $ game&voGameEditUrl .~ path

  where
    core :: MonadIO m => GameId -> ReaderT SqlBackend m VOGame
    core gameId = do
        game <- get404 gameId
        scores <- selectList [ScoreGameId ==. gameId] []
                    >>= pure . L.sortBy (
                            \r l -> let r' = scoreQuarter $ toRecord r
                                        l' = scoreQuarter $ toRecord l
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
          gameName = ""
        , gamePlace = ""
        , gameTeamAName = ""
        , gameTeamBName = ""
        , gameDate = now
      }
    , _voGameEditUrl = ""
    , _voGameScore = [
          Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = First
            , scoreTeamAPoint  = 0
            , scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Second
            , scoreTeamAPoint  = 0
            , scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Third
            , scoreTeamAPoint  = 0
            , scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Fourth
            , scoreTeamAPoint  = 0
            , scoreTeamBPoint  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Extension
            , scoreTeamAPoint  = 0
            , scoreTeamBPoint  = 0
          }
      ]
  }
