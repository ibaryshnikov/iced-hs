module Iced.Attribute.Alignment where

data Alignment = Start | Center | End

class UseAlignment a where
  alignItems :: Alignment -> a
