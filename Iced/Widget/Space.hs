{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Space (
  horizontalSpace,
  verticalSpace,
) where

import Foreign

import Iced.Element

data NativeSpace
type SelfPtr = Ptr NativeSpace
type Attribute = SelfPtr -> IO SelfPtr

foreign import ccall safe "new_horizontal_space"
  new_horizontal_space :: IO (SelfPtr)

foreign import ccall safe "new_vertical_space"
  new_vertical_space :: IO (SelfPtr)

foreign import ccall safe "space_into_element"
  space_into_element :: SelfPtr -> IO (ElementPtr)

data SpaceKind = Horizontal | Vertical

data Space = Space { attributes :: [Attribute], kind :: SpaceKind }

instance IntoNative Space where
  toNative details = do
    let constructor = kindToConstructor details.kind
    selfPtr <- constructor
    updatedSelf <- applyAttributes selfPtr details.attributes
    space_into_element updatedSelf

kindToConstructor :: SpaceKind -> IO (SelfPtr)
kindToConstructor kind = case kind of
  Horizontal -> new_horizontal_space
  Vertical -> new_vertical_space

horizontalSpace :: [Attribute] -> Element
horizontalSpace attributes = let kind = Horizontal in pack Space { .. }

verticalSpace :: [Attribute] -> Element
verticalSpace attributes = let kind = Vertical in pack Space { .. }
