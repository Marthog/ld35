{-# LANGUAGE TypeFamilies #-}

module Draw (
    Picture
    , Drawable(..)
    , translate
    , rotate
    , scale
    , unitCircle
    , circle
    , ellipse
    , transform
    , combine
    , color
) where

import ClassyPrelude
import qualified Graphics.Gloss as G
import Graphics.Gloss(Picture(..))
import Math

translate = uncurry G.translate . toTuple
rotate = G.rotate . radToDeg
scale = G.scale

circle = G.circleSolid
unitCircle = G.circleSolid 1
ellipse w h = scale w h unitCircle
color = G.color

combine :: (MonoFoldable t, Element t ~ Picture) => t -> Picture
combine = G.pictures . toList

class Drawable d where
    draw :: d -> Picture


transform :: Vec2 -> Float -> Picture -> Picture
transform position rotation = translate position . rotate rotation
