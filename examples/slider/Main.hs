{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Widget

data Model = Model
  { vertical :: Int
  , horizontal :: Int
  }

data Message
  = ChangedVertical Int
  | ChangedHorizontal Int
  | ReleasedVertical
  | ReleasedHorizontal

update :: Message -> Model -> Model
update (ChangedVertical value) model = model{vertical = value}
update (ChangedHorizontal value) model = model{horizontal = value}
update ReleasedVertical model = model{horizontal = model.vertical}
update ReleasedHorizontal model = model{vertical = model.horizontal}

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
    column
      [alignX Center, spacing 10, width (Fixed 150)]
      [ label "Vertical" model.vertical
      , label "Horizontal" model.horizontal
      , spaceHeight (Fixed 20)
      , verticalSlider
          [height (Fixed 150), onRelease ReleasedVertical]
          0
          100
          model.vertical
          ChangedVertical
      , spaceHeight (Fixed 20)
      , slider
          [width (Fixed 150), onRelease ReleasedHorizontal]
          0
          100
          model.horizontal
          ChangedHorizontal
      ]

label :: String -> Int -> Element
label prefix value =
  row
    []
    [ text [] prefix
    , horizontalSpace
    , text [] $ show value
    ]

main :: IO ()
main = Iced.run [] "Slider" model update view
 where
  model = Model{vertical = 50, horizontal = 50}
