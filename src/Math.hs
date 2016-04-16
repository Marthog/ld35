{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}

module Math (
    VecN
    ,Vec2(..)
    ,vLengthSq ,vLength
    ,normalize
    ,uncheckedNormalize
    ,vMap, vZip, vFold
    ,dot
    , (*|), (|*)
    , (|+|), (|-|), (-|)
    , toTuple
    , rotateToUnit
    , degToRad, radToDeg
) where

import ClassyPrelude
import Foreign


class VecN v where
    vMap :: (Float -> Float) -> v -> v
    vZip :: (Float -> Float -> Float) -> v -> v -> v
    vFold :: (Float -> Float -> Float) -> v -> Float

{-# INLINE (*|) #-}
infixl 7 *|
(*|) :: VecN v => Float -> v -> v
s *| v = vMap (*s) v

{-# INLINE (|*) #-}
infixl 7 |*
(|*) :: VecN v => v -> Float -> v
v |* s = s *| v

{-# INLINE (|+|) #-}
infixl 6 |+|
(|+|) :: VecN v => v -> v -> v
(|+|) = vZip (+)

{-# INLINE (|-|) #-}
infixl 6 |-|
(|-|) :: VecN v => v -> v -> v
(|-|) = vZip (-)

{-# INLINE (-|) #-}
(-|) :: VecN v => v -> v
(-|) = vMap negate

{-# INLINE vLengthSq #-}
vLengthSq :: VecN v => v -> Float
vLengthSq = vFold (+) . vMap (\x -> x*x)

{-# INLINE vLength #-}
vLength :: VecN v => v -> Float
vLength = sqrt . vLengthSq

{-# INLINE normalize #-}
normalize :: VecN v => v -> Maybe v
normalize v = if len==0 then 
        Nothing
    else
        Just $ (1/len) *| v
    where len = vLength v

{-# INLINE uncheckedNormalize #-}
uncheckedNormalize :: VecN v => v -> v
uncheckedNormalize v = (1/vLength v) *| v

{-# INLINE dot #-}
dot :: VecN v => v -> v -> Float
dot a b = vFold (+) $ vZip (*) a b

toTuple :: Vec2 -> (Float, Float)
toTuple (Vec2 a b) = (a,b)

rotateToUnit :: Float -> Vec2
rotateToUnit r = Vec2 (cos r) (sin r)

data Vec2 = Vec2 !Float !Float
    deriving (Show, Eq)

instance VecN Vec2 where
    {-# SPECIALIZE instance VecN Vec2 #-}
    vMap f (Vec2 x y) = Vec2 (f x) (f y)
    vZip f (Vec2 x0 y0) (Vec2 x1 y1) = Vec2 (f x0 x1) (f y0 y1)
    vFold f (Vec2 x y) = f x y


radToDeg x = x * 180 / pi
degToRad x = x * pi / 180
