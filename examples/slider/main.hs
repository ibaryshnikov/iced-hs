{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Widget

data Model = Model {
  vertical :: Int,
  horizontal :: Int
}

data Message
  = ChangedVertical Int
  | ChangedHorizontal Int
  | ReleasedVertical
  | ReleasedHorizontal

update :: Model -> Message -> Model
update model (ChangedVertical value) = model { vertical = value }
update model (ChangedHorizontal value) = model { horizontal = value }
update model ReleasedVertical = model { horizontal = model.vertical }
update model ReleasedHorizontal = model { vertical = model.horizontal }

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  column [alignItems Center, spacing 10, width (Fixed 150)] [
    label "Vertical" model.vertical,
    label "Horizontal" model.horizontal,
    spaceHeight (Fixed 20),
    verticalSlider [height (Fixed 150), onRelease ReleasedVertical]
      0 100 model.vertical ChangedVertical,
    spaceHeight (Fixed 20),
    slider [width (Fixed 150), onRelease ReleasedHorizontal]
      0 100 model.horizontal ChangedHorizontal
  ]

label :: String -> Int -> Element
label prefix value =
  row [] [
    text [] prefix,
    horizontalSpace,
    text [] $ show value
  ]

main :: IO ()
main = Iced.run [] "Slider" model update view
  where
    model = Model { vertical = 50, horizontal = 50 }
