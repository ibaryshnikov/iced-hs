{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Iced.Attribute.Padding where

data Padding = Padding {
  top :: Float,
  right :: Float,
  bottom :: Float,
  left :: Float
}

class PaddingToAttribute padding attribute where
  paddingToAttribute :: padding -> attribute

class UsePadding a where
  padding :: Float -> a

instance PaddingToAttribute Padding attribute => UsePadding attribute where
  padding v = paddingToAttribute $ Padding v v v v

class UsePadding2 attribute where
  padding2 :: Float -> Float -> attribute

instance PaddingToAttribute Padding attribute => UsePadding2 attribute where
  padding2 a b = paddingToAttribute Padding {
    top = a,
    right = b,
    bottom = a,
    left = b
  }

class UsePadding4 attribute where
  padding4 :: Float -> Float -> Float -> Float -> attribute

instance PaddingToAttribute Padding attribute => UsePadding4 attribute where
  padding4 top right bottom left = paddingToAttribute Padding { .. }
