{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import ClassyPrelude
import Graphics.Gloss

someFunc :: IO ()
someFunc = putStrLn "someFunc"
