{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where


import ClassyPrelude
import CommandLine

someFunc :: IO ()
someFunc = putStrLn "someFunc"
