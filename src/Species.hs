module Species (
    Unit(..)
    , updateUnit
) where

import ClassyPrelude
import Draw
import Math

data Unit = Unit {
    speed       :: !Float
    , slength   :: !Float
    , width     :: !Float
    , position  :: !Vec2
    , rotation  :: !Float
    }
    deriving (Show)


instance Drawable Unit where
    draw Unit{..} = transform position rotation $ ellipse slength width

updateUnit :: Float -> Unit -> Unit
updateUnit time u@Unit{..} = u{ position=newpos }
    where
        newpos = position |+| speed *| rotateToUnit rotation

testUnit = Unit {
    speed = 1.0
    , slength = 10.0
    , width = 5
    , position = Vec2 0 0
    , rotation = 0
    }
