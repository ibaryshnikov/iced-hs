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
type AttributeFn = SelfPtr -> IO SelfPtr

data Attribute

foreign import ccall safe "horizontal_space_new"
  horizontal_space_new :: IO (SelfPtr)

foreign import ccall safe "vertical_space_new"
  vertical_space_new :: IO (SelfPtr)

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

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr _attribute = pure selfPtr -- no attributes currently

kindToConstructor :: SpaceKind -> IO (SelfPtr)
kindToConstructor kind = case kind of
  Horizontal -> horizontal_space_new
  Vertical -> vertical_space_new

horizontalSpace :: [Attribute] -> Element
horizontalSpace attributes = let kind = Horizontal in pack Space { .. }

verticalSpace :: [Attribute] -> Element
verticalSpace attributes = let kind = Vertical in pack Space { .. }
