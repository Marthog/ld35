module Lib
    ( someFunc
    ) where


import ClassyPrelude
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Control.Monad.State
import Game

windowWidthI = 800              :: Int
windowHeightI = 600             :: Int

someFunc :: IO ()
someFunc = do
    let window = InWindow "Ludum Dare 35" (windowWidthI, windowHeightI) (100, 100)
    let game = newGame windowWidthI windowHeightI
    play window white 60 game drawGame (execState . input) (execState . updateGame)


input :: Event -> State Game ()
--input (EventResize s) = resize s -- game{view=resize s (view game)}


-- special key for logging position (useful for tutorial levels)
{-
input (EventKey (SpecialKey KeySpace) Up _ _)   game@Game{..} = do
    print $ printInt x ++ ", " ++ printInt y
    return game
    where
        (x,y) = position player
input (EventResize s) game          = return $ game{view=resize s (view game)}
input (EventKey key state _ _) game@Game{} = return $ keyPress key state game
input (EventKey (SpecialKey key) Up _ _) game@GameOver{..} =
    return $ case key of
        KeyUp   -> game{level=level+1}
        KeyDown -> game{level=max 0 $ level-1}
        KeyEnter -> let game = startGame view level
                    in execState (loadLevel level) game
        _       -> game
input event game = return $ game
-}

input event = return ()

printInt :: Float -> String
printInt a = show (round a)

upButtons = [SpecialKey KeyUp]
downButtons = [SpecialKey KeyDown]


keyPress key state game = game

