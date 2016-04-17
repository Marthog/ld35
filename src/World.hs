{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module World (
    World
    , drawWorld
    , emptyWorld
    , addRandom
    , updateWorld
    , addPlayer
    , units
    --, player
) where

import ClassyPrelude hiding (snoc)
import Draw
import Math
import Species
import Control.Lens as Lens
import Control.Monad.State(runState,gets, State, state)
import System.Random
import Graphics.Gloss.Data.Color
import Data.Vector((!))
import Input

data Rectangle = Rectangle !Vec2 !Vec2
    deriving (Show)

isInRectangle (Vec2 px py) (Rectangle (Vec2 x0 y0) (Vec2 x1 y1)) =
    px>=x0 && py<=x1 && py>=y0 && y1<=py


data Particle = Particle {
    position    :: !Vec2
    , ptype     :: !ParticleType
    }
    deriving (Show)

data ParticleType = Food !Float
    | DNA Float
    | Solid
    deriving (Show)


instance Drawable Particle where
    draw Particle{..} = translate position $ circle 10


data World = World {
    _units         :: !(Vector Unit)
    , _particles     :: !(Vector Particle)
    , _view          :: !Rectangle
    , _randomGen     :: !StdGen
}
    deriving (Show)


makeLenses ''World


--player :: Lens _ _ _ _
--player :: Functor f => (Unit -> f0 Unit) -> World -> f0 World
--player = units . _head

emptyWorld = World {
    _units = empty
    , _particles = empty
    , _view = Rectangle (Vec2 0 0) (Vec2 0 0)
    , _randomGen = mkStdGen 1
}

drawWorld World{..} = combine
    ( map draw _particles 
    ++ map draw _units
    )

updateWorld :: Float -> ButtonState -> State World ()
updateWorld time btnstate = do
    -- player . rotation += (pi * time)
    allUnits <- gets _units
    units . _tail . each & zoom $ runAI time allUnits
    zoom (units.each) (updateUnit time)
    updateAllUnits

updateAllUnits :: State World ()
updateAllUnits = do
    vec <- gets _units
    let playerHealth = _health $ vec ! 0
    when (playerHealth <= 0) (error "player died")
    newVec <- filterM check vec
    units .= newVec 
    where
        check u@Unit{..} = if _health>0 then
                    return True
                else do
                    spawnParticles _position (mass u)
                    return False

spawnParticles pos n = particles %= (++ part)
    where part = [
                Particle{position=pos, ptype=Food n}
            ]



randR :: Random a => a -> a -> State World a
randR a b = zoom randomGen $ state $ randomR (a,b)

rand :: Random a => State World a
rand = zoom randomGen $ state random

addPlayer :: State World ()
addPlayer = units %= (defaultUnit <|)

randVec2 (Vec2 x0 y0) (Vec2 x1 y1) = do
    x <- randR x0 x1
    y <- randR y0 y1
    return $ Vec2 x y


addRandom :: State World ()
addRandom = do
    rrotation <- randR 0 (2*pi)
    pos <- randVec2 (Vec2 (-100) (-100)) (Vec2 100 100)
    rratio <- sqrt <$> randR 2 6
    r <- rand
    g <- rand
    b <- rand
    rdam <- randR 4 20
    units %= (|> defaultUnit{
        _rotation = rrotation
        , _width = 10/rratio
        , _slength = 10*rratio
        , _position = pos
        , _color = makeColor r g b 1
        , _damage = rdam
    })

