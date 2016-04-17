{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Species (
    AI(..)
    , Unit(..)
    , updateUnit
    , defaultUnit
    , speed, slength, width, position, health
    , rotation, Species.color
    , runAI
    , mass
    , turn, move, attack
    , ai
    , worldSize
    , attackOthers
) where

import ClassyPrelude
import Draw
import Math
import Control.Lens as Lens
import Graphics.Gloss.Data.Color
import Control.Monad.Random
import Control.Monad.State
import Safe(fromJustDef)
import Data.Vector((!))

data AI = AI {
    _turn   :: !Float
    , _move   :: !Float
    , _attack :: !Float
}
    deriving (Show)

makeLenses ''AI

data Unit = Unit {
    _speed       :: !Float
    , _slength   :: !Float
    , _width     :: !Float
    , _position  :: !Vec2
    , _rotation  :: !Float
    , _color        :: !Color
    , _damage       :: !Float
    , _health       :: !Float
    , _food         :: !Float
    , _ai        :: !AI
    , _dna       :: !(Vector Float)
    }
    deriving (Show)

makeLenses ''Unit

instance Drawable Unit where
    draw Unit{..} = Draw.transform _position _rotation $ Draw.color _color $ ellipse _slength _width

updateUnit :: Float -> State Unit ()
updateUnit time = do
    t <- use $ ai.turn
    rot <- rotation <+= t * time * turnSpeed
    pos <- use position
    sp <- use speed
    mv <- use $ ai.move
    l <- use slength
    let newpos = pos |+| (sp * mv) *| rotateToUnit rot
    let maxlen = vLength newpos
    let mincircle = worldSize-l/2
    position .= if maxlen<mincircle then newpos else
        fromJustDef (Vec2 0 0) (normalize newpos) |* mincircle


--maxHealth unit = _dna unit ! 0


worldSize = 200

turnSpeed = 2.0

defaultUnit = Unit {
    _speed = 1.0
    , _slength = 10.0
    , _width = 5
    , _position = Vec2 0 0
    , _rotation = 0
    , _color = black
    , _damage = 4
    , _health = 10
    , _food = 50
    , _ai = AI {
        _turn = 0
        , _move = 0
        , _attack = 0
        }
    , _dna = replicate 10 0
    }



mass :: Unit -> Float
mass Unit{..} = _health + 0.5*_food + 5

distance :: Unit -> Unit -> Float
distance a b = vLength $ _position a |-| _position b

relMovement :: Unit -> Unit -> Vec2
relMovement self other = fromJustDef diff lvec |* dam
    where 
        diff = _position self |-| _position other
        lvec = (|* (1/vLength diff)) <$> normalize diff
        dam = _damage self - _damage other


eat :: Float -> State Unit ()
eat f = 
    food += f

dnaDistance :: Unit -> Unit -> Float
dnaDistance a b = sum $ zipWith ((-) `on` abs) (_dna a) (_dna b)

attackOthers :: Unit -> State (Vector Unit) ()
attackOthers attacker = modify $ map $ \unit ->
    if distance attacker unit < 10 && dnaDistance unit attacker>10 then
        unit { _health = _health unit - _health attacker }
    else
        unit

runAI :: Float -> Vector Unit -> State Unit ()
runAI time others = do
    self <- get
    center <- forceCenter
    let target = foldl' (|+|) center $ map (relMovement self) others
    let targetAngle = angleBetween (Vec2 1 0) target
    rotation .= targetAngle
    ai.move .= 1.0

-- | Force into the center.
forceCenter :: State Unit Vec2
forceCenter = (|*((-1)/100)) <$> use position

