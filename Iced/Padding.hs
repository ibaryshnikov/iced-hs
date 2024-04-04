{-# LANGUAGE RecordWildCards #-}

module Iced.Padding where

data Padding = Padding {
  top :: Float,
  right :: Float,
  bottom :: Float,
  left :: Float
}

class UsePadding attribute where
  paddingToAttribute :: Padding -> attribute
  padding :: Float -> attribute
  padding value = paddingToAttribute Padding {
    top = value,
    right = value,
    bottom = value,
    left = value
  }
  padding2 :: Float -> Float -> attribute
  padding2 a b = paddingToAttribute Padding {
    top = a,
    right = b,
    bottom = a,
    left = b
  }
  padding4 :: Float -> Float -> Float -> Float -> attribute
  padding4 top right bottom left = paddingToAttribute Padding { .. }
