{-# LANGUAGE RecordWildCards #-}

module Iced.Padding where

data Padding = Padding {
  top :: Float,
  right :: Float,
  bottom :: Float,
  left :: Float
}

padding :: Float -> Padding
padding a = Padding {
    top = a,
    right = a,
    bottom = a,
    left = a
  }

sides :: Float -> Float -> Padding
sides a b = Padding {
    top = a,
    right = b,
    bottom = a,
    left = b
  }

full :: Float -> Float -> Float -> Float -> Padding
full top right bottom left = Padding { .. }
