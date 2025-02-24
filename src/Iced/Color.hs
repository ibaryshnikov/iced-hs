{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Iced.Color (
  Color (..),
  rgb,
  rgba,
  rgb8,
  rgba8,
) where

import Data.Word

data Color = Color
  { r :: Float
  , g :: Float
  , b :: Float
  , a :: Float
  }

rgb :: Float -> Float -> Float -> Color
rgb r g b = rgba r g b 1

rgba :: Float -> Float -> Float -> Float -> Color
rgba r g b a = Color{..}

rgb8 :: Word8 -> Word8 -> Word8 -> Color
rgb8 r g b = rgba8 r g b 1

rgba8 :: Word8 -> Word8 -> Word8 -> Float -> Color
rgba8 inR inG inB a = rgba r g b a
 where
  r = toFloat inR
  g = toFloat inG
  b = toFloat inB

toFloat :: Word8 -> Float
toFloat word = (fromIntegral word) / 255
