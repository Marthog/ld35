{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Input (
    ButtonState(..)
    , leftPressed, rightPressed, 
) where

import ClassyPrelude hiding (snoc)
import Control.Lens as Lens
import Control.Monad.State(runState,gets, State, state)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game as G

data ButtonState = ButtonState {
    _leftPressed    :: !Bool
    , _rightPressed :: !Bool
    }

makeLenses ''ButtonState
