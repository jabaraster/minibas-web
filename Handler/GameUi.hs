module Handler.GameUi where

import Import

import Jabara.Yesod.Util (getResourcePath)

getGameIndexUiR :: Handler Html
getGameIndexUiR = defaultLayout $ do
    addScript $ StaticR js_games_js
    setTitle' "試合一覧"
    $(widgetFile "games")

getNewGameUiR :: Handler Html
getNewGameUiR = defaultLayout $ do
    addScript $ StaticR js_new_game_js
    setTitle' "新しい試合"
    $(widgetFile "new-game")

getGameUiR :: GameId -> Handler Html
getGameUiR gameId = defaultLayout $ do
    jsonPath <- getResourcePath $ GameR $ gameId
    addScript $ StaticR js_game_js
    setTitle' "試合"
    $(widgetFile "game")
