{-# OPTIONS_GHC -fno-warn-orphans #-}

module Minibas.Web where

import Model (Game)

import ClassyPrelude.Yesod
import Jabara.Yesod.Util (tc, ttc)

instance ToContent (Entity Game) where toContent = tc
instance ToTypedContent (Entity Game) where toTypedContent = ttc

instance ToContent [Entity Game] where toContent = tc
instance ToTypedContent [Entity Game] where toTypedContent = ttc
