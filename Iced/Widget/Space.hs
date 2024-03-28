{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Iced.Widget.Space (
  horizontalSpace,
  verticalSpace,
) where

import Foreign

import Iced.Element

data NativeSpace
type SpacePtr = Ptr NativeSpace

foreign import ccall safe "new_horizontal_space"
  new_horizontal_space :: IO (SpacePtr)

foreign import ccall safe "new_vertical_space"
  new_vertical_space :: IO (SpacePtr)

foreign import ccall safe "space_into_element"
  space_into_element :: SpacePtr -> IO (ElementPtr)

data SpaceKind = Horizontal | Vertical

data Space = Space { kind :: SpaceKind, attributes :: [SpacePtr -> IO SpacePtr] }

instance IntoNative Space where
  toNative details = do
    let constructor = kindToConstructor details.kind
    spacePtr <- constructor
    updatedSpace <- applyAttributes spacePtr details.attributes
    space_into_element updatedSpace

kindToConstructor :: SpaceKind -> IO (SpacePtr)
kindToConstructor kind = case kind of
  Horizontal -> new_horizontal_space
  Vertical -> new_vertical_space

horizontalSpace :: [SpacePtr -> IO SpacePtr] -> Element
horizontalSpace attributes = pack Space { kind = Horizontal, attributes = attributes }

verticalSpace :: [SpacePtr -> IO SpacePtr] -> Element
verticalSpace attributes = pack Space { kind = Vertical, attributes = attributes }
