module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    addScript $ StaticR js_home_js
    setTitle' "Home"
    $(widgetFile "home")
