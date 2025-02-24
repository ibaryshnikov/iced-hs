{-# LANGUAGE NoFieldSelectors #-}

module Iced.Attribute.Padding where

data Padding = Padding
  { top :: Float
  , right :: Float
  , bottom :: Float
  , left :: Float
  }

paddingFromOne :: Float -> Padding
paddingFromOne v = Padding v v v v

paddingFromTwo :: Float -> Float -> Padding
paddingFromTwo a b =
  Padding
    { top = a
    , right = b
    , bottom = a
    , left = b
    }

class UsePadding a where
  padding :: Float -> a

class UsePadding2 attribute where
  padding2 :: Float -> Float -> attribute

class UsePadding4 attribute where
  padding4 :: Float -> Float -> Float -> Float -> attribute
