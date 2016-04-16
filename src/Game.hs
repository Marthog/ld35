{-# LANGUAGE TemplateHaskell #-}

module Game (
    Game(..)
    , drawGame
    , newGame
    , updateGame
    --, resize
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

--resize :: (Float, Float) -> State Game ()
--resize (x,y) = viewPort %= \v -> v{viewPortTranslate = (fromIntegral x, fromIntegral y)}

newGame :: Int -> Int -> Game
newGame width height = Game{
            _world=world
            , _viewPort=viewPortInit{ viewPortScale = 4 }
            }
    where
        world = execState (do
            addPlayer
            replicateM_ 10 addRandom 
            ) emptyWorld

updateGame :: Float -> State Game ()
updateGame time = 
    zoom world $ updateWorld time
