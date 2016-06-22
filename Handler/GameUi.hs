module Handler.GameUi where

import Import

getGameIndexUiR :: Handler Html
getGameIndexUiR = defaultLayout $ do
    addScript $ StaticR js_game_index_js
    setTitle' "ゲーム"
    $(widgetFile "game-index")

getNewGameUiR :: Handler Html
getNewGameUiR = defaultLayout $ do
    addScript $ StaticR js_new_game_js
    setTitle' "新しいゲーム"
    $(widgetFile "new-game")

getGameUiR :: GameId -> Handler Html
getGameUiR gameId = defaultLayout $ do
    addScript $ StaticR js_game_js
    setTitle' "ゲーム"
    $(widgetFile "game")
