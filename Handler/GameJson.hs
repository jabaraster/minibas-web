{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.GameJson where

import Import

import           Control.Lens
import qualified Data.List as L (sortBy)
import           Jabara.Persist.Util (dummyKey, toRecord, toKey)

getGameIndexR :: Handler [Entity Game]
getGameIndexR = runDB $ selectList [] [Asc GameDate]

putGameIndexR :: Handler ()
putGameIndexR = do
    req::VOGame <- requireJsonBody
    gameId <- runDB $ do
                  gameId <- insert $ toRecord $ req^.voGameGame
                  _      <- mapM (\s -> insert (toRecord s) { scoreGameId = gameId }) $ req^.voGameScore
                  pure gameId
    sendResponseCreated $ GameUiR gameId

getGameR :: GameId -> Handler VOGame
getGameR gameId = runDB $ core gameId
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
        pure $ VOGame { _voGameGame = Entity gameId game, _voGameScore = scores }

postGameR :: GameId -> Handler ()
postGameR gameId = undefined

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
    , _voGameScore = [
          Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = First
            , scoreTeamAScore  = 0
            , scoreTeamBScore  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Second
            , scoreTeamAScore  = 0
            , scoreTeamBScore  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Third
            , scoreTeamAScore  = 0
            , scoreTeamBScore  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Fourth
            , scoreTeamAScore  = 0
            , scoreTeamBScore  = 0
          }
        , Entity dummyKey $ Score {
              scoreGameId = dummyKey
            , scoreQuarter = Extension
            , scoreTeamAScore  = 0
            , scoreTeamBScore  = 0
          }
      ]
  }
