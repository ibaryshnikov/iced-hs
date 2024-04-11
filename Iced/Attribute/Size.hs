module Iced.Attribute.Size where

class UseSize a where
  size :: Float -> a
