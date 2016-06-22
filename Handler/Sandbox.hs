module Handler.Sandbox where

import Import

getSandboxR :: Handler Html
getSandboxR = defaultLayout $ do
    setTitle' "サンドボックス"
    addScript $ StaticR js_sandbox_js
    $(widgetFile "sandbox")
