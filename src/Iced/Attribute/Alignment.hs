module Iced.Attribute.Alignment where

data Alignment = Start | Center | End

class UseAlignX a where
  alignX :: Alignment -> a

class UseAlignY a where
  alignY :: Alignment -> a
