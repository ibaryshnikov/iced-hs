{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Iced.Attribute.Padding where

data Padding = Padding {
  top :: Float,
  right :: Float,
  bottom :: Float,
  left :: Float
}

class UsePadding a where
  padding :: Float -> a

class PaddingToAttribute attribute where
  paddingToAttribute :: Padding -> attribute

class PaddingToAttribute attribute => UsePadding2 attribute where
  padding2 :: Float -> Float -> attribute
  padding2 a b = paddingToAttribute Padding {
    top = a,
    right = b,
    bottom = a,
    left = b
  }

instance PaddingToAttribute attribute => UsePadding2 attribute

class PaddingToAttribute attribute => UsePadding4 attribute where
  padding4 :: Float -> Float -> Float -> Float -> attribute
  padding4 top right bottom left = paddingToAttribute Padding { .. }

instance PaddingToAttribute attribute => UsePadding4 attribute
