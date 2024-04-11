module Iced.Size where

class UseSize a where
  size :: Float -> a
