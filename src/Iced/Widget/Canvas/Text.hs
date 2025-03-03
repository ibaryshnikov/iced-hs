{-# LANGUAGE NoFieldSelectors #-}

module Iced.Widget.Canvas.Text where

import Iced.Attribute.Alignment
import Iced.Attribute.LineHeight
import Iced.Attribute.Text
import Iced.Color

data Text = Text
  { content :: String
  , x :: Float
  , y :: Float
  , color :: Color
  , size :: Float
  , lineHeight :: LineHeight
  , horizontal :: Alignment
  , vertical :: Alignment
  , shaping :: Shaping
  }
