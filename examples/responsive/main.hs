{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Size
import Iced.Theme
import Iced.Widget
import Iced.Widget.Container (BasicStyle(..))

data Model = Model { value_ :: Int }

data Message = WidthChanged Int

update :: Model -> Message -> Model
update model (WidthChanged value_) = model { value_ = value_ }

view :: Model -> Element
view model =
  center [] $
  column [alignItems Center] [
    text [size 20] "Contents disappear if width is less than 200",
    spaceHeight (Fixed 20),
    row [alignItems Center, spacing 6] [
      text [width (Fixed 200)] $ "Container width: " ++ show model.value_,
      slider [width (Fixed 200)] 0 500 model.value_ WidthChanged
    ],
    spaceHeight (Fixed 20),
    container containerAttributes $ responsive label
  ]
  where
    containerAttributes = [style RoundedBox, width containerWidth, height (Fixed 100)]
    containerWidth = Fixed $ fromIntegral model.value_

label :: Size -> Element
label s = column [] $ if s.width < 200 then [] else [
    text [] "Responsive widget size",
    text [] $ "width   " ++ show s.width,
    text [] $ "height  " ++ show s.height
  ]

main :: IO ()
main = Iced.run [theme SolarizedLight] "Responsive" model update view
  where model = Model { value_ = 250 }
