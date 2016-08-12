module Handler.League where

import Import

getLeagueIndexR :: Handler [Entity League]
getLeagueIndexR = runDB $ selectList [] [Desc LeagueId]
