{-# LANGUAGE TemplateHaskell #-}

module Game (
    Game(..)
    , drawGame
    , newGame
    , updateGame
    , buttonState
    , registerKey
    --, resize
) where 

import ClassyPrelude
import Math
import World
import Draw
import Graphics.Gloss.Data.ViewPort
import Control.Lens
import Control.Monad.State
import Input
import Graphics.Gloss.Interface.Pure.Game as G
import Species

data Game = Game {
    _world       :: !World
    , _viewPort  :: !ViewPort
    , _buttonState :: !ButtonState
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
            , _buttonState=defaultButtonState
            }
    where
        world = execState (do
            addPlayer
            replicateM_ 10 addRandom 
            ) emptyWorld

updateGame :: Float -> State Game ()
updateGame time = do
    btn <- use buttonState
    zoom world $ updateWorld time btn


btof True = 1.0
btof False = 0.0

keyPressed :: KeyState -> Bool
keyPressed G.Up = False
keyPressed G.Down = True


registerKey :: Key -> KeyState -> State Game ()
registerKey key state' = do
    case key of
        SpecialKey KeyUp        -> ctrl . move .= btof state
        SpecialKey KeyLeft      -> btn . leftPressed .= state
        SpecialKey KeyRight     -> btn . rightPressed .= state
        SpecialKey KeySpace     -> ctrl . attack .= not state
        _                       -> return ()

    b <- use buttonState
    ctrl . turn .= btof (_rightPressed b) - btof (_leftPressed b)

    where
        btn = buttonState
        ctrl = world . units . _head . ai
        state = keyPressed state'

defaultButtonState = ButtonState {
    _leftPressed = False
    , _rightPressed = False
}
