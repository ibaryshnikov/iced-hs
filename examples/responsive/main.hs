{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Size
import Iced.Widget

data Model = Model { value :: Float }

data Message = Inc | Dec

update :: Model -> Message -> Model
update model Inc = model { value = model.value + 10 }
update model Dec = model { value = model.value - 10 }

view :: Model -> Element
view model =
  container [centerX, centerY, width Fill, height Fill] $
  column [alignItems Center] [
    text [size 20] "Contents disappear if width is less than 200",
    spaceHeight (Fixed 20),
    row [alignItems Center, spacing 6] [
      text [width (Fixed 200)] $ "Container width: " ++ show model.value,
      button [onPress Dec] "-",
      button [onPress Inc] "+"
    ],
    spaceHeight (Fixed 20),
    container [width (Fixed model.value), height (Fixed 100)] $ responsive label
  ]

label :: Size -> Element
label s = column [] $ if s.width < 200 then [] else [
    text [] "Responsive widget size",
    text [] $ "width   " ++ show s.width,
    text [] $ "height  " ++ show s.height,
    verticalSpace,
    row [] [
      text [] "left",
      horizontalSpace,
      text [] "right"
    ]
  ]

main :: IO ()
main = Iced.run [] "Responsive" model update view
  where model = Model { value = 250 }
