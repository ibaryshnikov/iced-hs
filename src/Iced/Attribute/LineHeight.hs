module Iced.Attribute.LineHeight where

data LineHeight = Relative Float | Absolute Float

class UseLineHeight a where
  lineHeight :: LineHeight -> a
