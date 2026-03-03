{-# LANGUAGE ExistentialQuantification #-}

module Modules.TV.Api
  ( TvService,
    makeTvService,
    currentHomeTVState,
    subscribeHomeTV,
  )
where

import ClassyPrelude (IO, Int, Maybe, pure, (.))
import Control.Concurrent.STM (TChan)
import Controllers.Game.Model.ServerGame (ServerGameSnapshot)
import Modules.Games.Api (GameService)
import Modules.TV.Home (HomeTvService, HomeTvUpdate, makeHomeTvService)
import qualified Modules.TV.Home as Home
import Repository.GameRepository (GameRepository)

data TvService = TvService
  { tvServiceHomeTv :: HomeTvService
  }

makeTvService :: GameRepository a => a -> GameService -> Int -> IO TvService
makeTvService repo gameService refreshMinutes = do
  homeTv <- makeHomeTvService repo gameService refreshMinutes
  pure (TvService homeTv)

currentHomeTVState :: TvService -> IO (Maybe ServerGameSnapshot)
currentHomeTVState = Home.currentTVState . tvServiceHomeTv

subscribeHomeTV :: TvService -> IO (TChan HomeTvUpdate)
subscribeHomeTV = Home.subscribeHomeTV . tvServiceHomeTv
