{-# LANGUAGE TemplateHaskell #-}

module Game (
    Game(..)
    , drawGame
    , newGame
) where 

import ClassyPrelude
import Math
import World
import Draw
import Graphics.Gloss.Data.ViewPort
import Control.Lens
import Control.Monad.State

data Game = Game {
    _world       :: !World
    , _viewPort  :: !ViewPort
} 

makeLenses ''Game

drawGame :: Game -> Picture
drawGame Game{..} = applyViewPortToPicture _viewPort $ drawWorld _world

resize :: (Float, Float) -> State Game ()
resize p = viewPort %= \v -> v{viewPortTranslate = p}

newGame width height = Game{_world=emptyWorld, _viewPort=viewPortInit}
