{-# LANGUAGE TemplateHaskell #-}

module Species (
    Unit(..)
    , updateUnit
    , defaultUnit
    , speed, slength, width, position, health
    , rotation, Species.color
    , runAI
    , mass
    , AI
    , turn, move, attack
    , ai
) where

import ClassyPrelude
import Draw
import Math
import Control.Lens
import Graphics.Gloss.Data.Color
import Control.Monad.Random
import Control.Monad.State
import Safe(fromJustDef)

data AI = AI {
    _turn   :: !Float
    , _move   :: !Float
    , _attack :: !Bool
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
    let newpos = pos |+| (sp * mv) *| rotateToUnit rot
    position .= newpos


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
        , _attack = False
        }
    }


mass :: Unit -> Float
mass Unit{..} = _health + 0.5*_food + 5


speciesDistance :: Unit -> Unit -> Float
speciesDistance a b = 0


relMovement :: Unit -> Unit -> Vec2
relMovement self other = fromJustDef (Vec2 0 0) lvec |* dam
    where 
        diff = _position self |-| _position other
        lvec = (|* (1/vLength diff)) <$> normalize diff
        dam = _damage self - _damage other


eat :: Float -> State Unit ()
eat f = 
    food += f

attackOther :: Unit -> State Unit ()
attackOther attacker = do
    newhealth <- health <-= _damage attacker    
    when (newhealth <= 0) $ 
       return () 

runAI :: Float -> Vector Unit -> State Unit ()
runAI time others = do
    self <- get
    let target = foldl' (|+|) (Vec2 0 0) $ map (relMovement self) others
    let targetAngle = angleBetween (Vec2 1 0) target
    rotation .= targetAngle
