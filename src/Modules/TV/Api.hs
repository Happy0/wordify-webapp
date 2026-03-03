{-# LANGUAGE ExistentialQuantification #-}

module Modules.TV.Api
  ( TvService,
    makeTvService,
  )
where

import Repository.GameRepository (GameRepository)

data TvService = forall a. GameRepository a => TvService a

makeTvService :: GameRepository a => a -> TvService
makeTvService = TvService
