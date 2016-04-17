{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where


import ClassyPrelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Control.Monad.State
import Game
import Input
import Control.Lens

windowWidthI = 800              :: Int
windowHeightI = 600             :: Int

someFunc :: IO ()
someFunc = do
    let window = InWindow "Ludum Dare 35" (windowWidthI, windowHeightI) (100, 100)
    let game = newGame windowWidthI windowHeightI
    play window black 60 game drawGame (execState . input) (execState . updateGame)

input :: Event -> State Game ()
--input (EventResize s) = resize s -- game{view=resize s (view game)}
input (EventKey key state _ _) = registerKey key state
input event = return ()

printInt :: Float -> String
printInt a = show (round a)

upButtons = [SpecialKey KeyUp]
downButtons = [SpecialKey KeyDown]


keyPress key state game = game

