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
import Iced.Attribute.Padding

data NativeTooltip
type Self = Ptr NativeTooltip
type AttributeFn = Self -> IO Self
data NativeStyle
type StylePtr = Ptr NativeStyle
data Position = FollowCursor | Top | Bottom | LeftSide | RightSide

data Attribute = Gap Float | AddPadding Float | SnapWithViewport Bool -- | Style Style

-- content tooltip position
foreign import ccall safe "tooltip_new"
  tooltip_new :: ElementPtr -> ElementPtr -> CUChar -> IO Self

-- tooltip gap
foreign import ccall safe "tooltip_gap"
  tooltip_gap :: Self -> CFloat -> IO Self

-- tooltip padding
foreign import ccall safe "tooltip_padding"
  tooltip_padding :: Self -> CFloat -> IO Self

-- tooltip snap
foreign import ccall safe "tooltip_snap_within_viewport"
  tooltip_snap_within_viewport :: Self -> CBool -> IO Self

-- skip for now
-- tooltip style
foreign import ccall safe "tooltip_style"
  tooltip_style :: Self -> StylePtr -> IO Self

foreign import ccall safe "tooltip_into_element"
  into_element :: Self -> IO ElementPtr

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
    content <- elementToNative details.content
    tooltip <- elementToNative details.tooltipElement
    let position = positionToNative details.position
    tooltip_new content tooltip (CUChar position)
      >>= applyAttributes details.attributes
      >>= into_element

instance UseAttribute Self Attribute where
  useAttribute attribute = do
    case attribute of
      Gap value -> useGap value
      AddPadding value -> usePadding value
      SnapWithViewport snap -> useSnapWithViewport snap

instance UsePadding Attribute where
  padding value = AddPadding value

tooltip :: [Attribute] -> Element -> Element -> Position -> Element
tooltip attributes content tooltipElement position = pack Tooltip { .. }

gap :: Float -> Attribute
gap value = Gap value

useGap :: Float -> AttributeFn
useGap value self = do
  tooltip_gap self (CFloat value)

usePadding :: Float -> AttributeFn
usePadding value self = do
  tooltip_padding self (CFloat value)

snapWithViewport :: Bool -> Attribute
snapWithViewport snap = SnapWithViewport snap

useSnapWithViewport :: Bool -> AttributeFn
useSnapWithViewport snap self = do
  tooltip_snap_within_viewport self (fromBool snap)
