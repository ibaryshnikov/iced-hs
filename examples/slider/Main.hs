{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Widget

data Model = Model
  { verticalSize :: Int
  , horizontalSize :: Int
  }

data Message
  = ChangedVertical Int
  | ChangedHorizontal Int
  | ReleasedVertical
  | ReleasedHorizontal

update :: Message -> Model -> Model
update (ChangedVertical value) model = model{verticalSize = value}
update (ChangedHorizontal value) model = model{horizontalSize = value}
update ReleasedVertical model = model{horizontalSize = model.verticalSize}
update ReleasedHorizontal model = model{verticalSize = model.horizontalSize}

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
    column
      [alignX Center, spacing 10, width (Fixed 150)]
      [ makeLabel "Vertical" model.verticalSize
      , makeLabel "Horizontal" model.horizontalSize
      , space [height (Fixed 20)]
      , verticalSlider
          [height (Fixed 150), onRelease ReleasedVertical]
          0
          100
          model.verticalSize
          ChangedVertical
      , space [height (Fixed 20)]
      , slider
          [width (Fixed 150), onRelease ReleasedHorizontal]
          0
          100
          model.horizontalSize
          ChangedHorizontal
      ]

makeLabel :: String -> Int -> Element
makeLabel prefix value =
  row
    []
    [ text [] prefix
    , horizontal []
    , text [] $ show value
    ]

main :: IO ()
main = Iced.run [] "Slider" model update view
 where
  model = Model{verticalSize = 50, horizontalSize = 50}
