module Handler.Team where

import Import

getTeamIndexR :: Handler [Entity Team]
getTeamIndexR = runDB $ selectList [] [Asc TeamId]
