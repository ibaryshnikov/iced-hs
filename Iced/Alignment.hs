module Iced.Alignment where

data Alignment = Start | Center | End

class UseAlignment a where
  alignItems :: Alignment -> a
