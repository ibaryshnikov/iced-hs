module Iced.Attribute.SliderCommon where

class SliderCommon a where
  addDefault :: Int -> a
  step :: Int -> a
  shiftStep :: Int -> a
