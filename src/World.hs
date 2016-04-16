{-# LANGUAGE TypeFamilies #-}

module World (
    World
    , drawWorld
    , emptyWorld
) where

import ClassyPrelude
import Draw
import Math
import Species(Unit, updateUnit)

data Rectangle = Rectangle !Vec2 !Vec2

data World = World {
    units           :: !(Vector Unit)
    , particles     :: !(Vector Particle)
}
    deriving (Show)

emptyWorld = World{units = empty, particles = empty}

drawWorld World{..} = combine (map draw particles ++ map draw units)

updateWorld time w@World{..} = w{units = updateUnit time `map` units}


data Particle = Particle {
    position    :: !Vec2
    , rotation  :: !Float
    , size      :: !Float
    , ptype     :: !ParticleType
    }
    deriving (Show)

data ParticleType = Food !Float
    | Solid
    deriving (Show)


instance Drawable Particle where
    draw Particle{..} = transform position rotation $ circle size


