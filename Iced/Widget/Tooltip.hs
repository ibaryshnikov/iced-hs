{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Iced.Widget.Tooltip (
  tooltip,
  gap,
  snapWithViewport,
  Position(..),
) where

import Data.Word
import Foreign
import Foreign.C.Types

import Iced.Element

data NativeTooltip
type SelfPtr = Ptr NativeTooltip
type AttributeFn = SelfPtr -> IO SelfPtr
data NativeStyle
type StylePtr = Ptr NativeStyle
data Position = FollowCursor | Top | Bottom | LeftSide | RightSide

data Attribute = Gap Float | AddPadding Float | SnapWithViewport Bool -- | Style Style

-- content tooltip position
foreign import ccall safe "tooltip_new"
  tooltip_new :: ElementPtr -> ElementPtr -> CUChar -> IO (SelfPtr)

-- tooltip gap
foreign import ccall safe "tooltip_gap"
  tooltip_gap :: SelfPtr -> CFloat -> IO (SelfPtr)

-- tooltip padding
foreign import ccall safe "tooltip_padding"
  tooltip_padding :: SelfPtr -> CFloat -> IO (SelfPtr)

-- tooltip snap
foreign import ccall safe "tooltip_snap_within_viewport"
  tooltip_snap_within_viewport :: SelfPtr -> CBool -> IO (SelfPtr)

-- skip for now
-- tooltip style
foreign import ccall safe "tooltip_style"
  tooltip_style :: SelfPtr -> StylePtr -> IO (SelfPtr)

foreign import ccall safe "tooltip_into_element"
  tooltip_into_element :: SelfPtr -> IO (ElementPtr)

data Tooltip = Tooltip {
  attributes :: [Attribute],
  content :: Element,
  tooltipElement :: Element,
  position :: Position
}

positionToNative :: Position -> Word8
positionToNative position = case position of
    FollowCursor -> 1
    Top -> 2
    Bottom -> 3
    LeftSide -> 4 -- to avoid overlap with Either
    RightSide -> 5 --

instance IntoNative Tooltip where
  toNative details = do
    contentPtr <- elementToNative details.content
    tooltipPtr <- elementToNative details.tooltipElement
    let position = positionToNative details.position
    selfPtr <- tooltip_new contentPtr tooltipPtr (CUChar position)
    updatedSelf <- applyAttributes selfPtr details.attributes
    tooltip_into_element updatedSelf

instance UseAttribute SelfPtr Attribute where
  useAttribute selfPtr attribute = do
    case attribute of
      Gap value -> useGap value selfPtr
      AddPadding value -> usePadding value selfPtr
      SnapWithViewport snap -> useSnapWithViewport snap selfPtr

tooltip :: [Attribute] -> Element -> Element -> Position -> Element
tooltip attributes content tooltipElement position = pack Tooltip { .. }

gap :: Float -> Attribute
gap value = Gap value

useGap :: Float -> AttributeFn
useGap value selfPtr = do
  tooltip_gap selfPtr (CFloat value)

usePadding :: Float -> AttributeFn
usePadding value selfPtr = do
  tooltip_padding selfPtr (CFloat value)

snapWithViewport :: Bool -> Attribute
snapWithViewport snap = SnapWithViewport snap

useSnapWithViewport :: Bool -> AttributeFn
useSnapWithViewport snap selfPtr = do
  tooltip_snap_within_viewport selfPtr (fromBool snap)
