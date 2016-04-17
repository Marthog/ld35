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
    , player
) where

import ClassyPrelude hiding (snoc)
import Draw
import Math
import Species
import Control.Lens as Lens
import Control.Monad.State(runState,gets, State, evalState, state,modify)
import System.Random
import Graphics.Gloss.Data.Color
import Data.Vector((!), tail)
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
    , _randomGen     :: !StdGen
}
    deriving (Show)


makeLenses ''World


player :: Applicative f => (Unit -> f Unit) -> World -> f World
player = units . _head

emptyWorld = World {
    _units = empty
    , _particles = empty
    , _randomGen = mkStdGen 1
}

drawWorld w@World{..} = trans $ combine
    ( [Draw.color white $ circle worldSize] ++
    map draw _particles 
    ++ map draw _units
    )
    where
        ppos = Species._position (_units ! 0)
        trans = translate (vMap negate ppos)

updateWorld :: Float -> ButtonState -> State World ()
updateWorld time btnstate = do
    -- player . rotation += (pi * time)
    allUnits <- use units
    units . _tail . each & zoom $ runAI time allUnits
    zoom (units.each) (updateUnit time)
    updateAllUnits time

updateAllUnits :: Float -> State World ()
updateAllUnits time = do
    vec <- gets _units
    let attacks = filter (\x -> _attack (_ai x)==1) vec
    --traceShowM (length attacks)
    forM_ attacks $ zoom units . attackOthers
    vec <- use units
    let playerHealth = _health $ vec ! 0
    when (playerHealth <= 0) (error "player died")
    newVec <- filterM check (tail vec)
    playr <- gets ((!0) . _units)
    units .= [playr] ++ newVec
    units.each.ai.attack -= time
    zoom units $ modify splitUnits
    where
        check u@Unit{..} = if _health>0 then
                    return True
                else do
                    player.speed += _speed/2
                    player.slength += (_slength / 3)
                    player.width += (_width / 3)
                    return False

splitUnits :: Vector Unit -> Vector Unit
splitUnits = concatMap (\u -> if _food u>=20 then 
                    [u] 
                else 
                    let n = u{_food = _food u / 2} 
                    in [n, n{_position = _position n |+| Vec2 5 0}])



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
    w <- rand
    h <- rand
    r <- rand
    g <- rand
    b <- rand
    rdam <- randR 4 20
    units %= (|> defaultUnit{
        _rotation = rrotation
        , _width = w+1
        , _slength = h+2
        , _position = pos
        , _color = makeColor r g b 1
        , _damage = rdam
    })

