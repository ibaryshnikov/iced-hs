module Main where

import Iced
import Iced.Attribute
import Iced.Color
import Iced.Widget
import Iced.Widget.Button qualified as Button

data Message = Click

update :: Int -> Message -> Int
update value Click = value + 1

greyButton :: [Button.AppearanceAttribute]
greyButton =
  [
    background (rgb8 192 192 192),
    border (rgb8 0 0 0) 1 4,
    textColor (rgb8 0 0 0),
    hovered [
      background (rgb8 200 200 200)
    ],
    pressed [
      background (rgb8 176 176 176)
    ],
    disabled [
      background (rgb8 160 160 160)
    ]
  ]

view :: Int -> Element
view value =
  container [centerX, centerY, width Fill, height Fill] $
  column [alignItems Center, spacing 10] [
    button [
      onPress Click,
      style greyButton
    ] "Active",
    text [size 50] $ show value,
    button [
      style greyButton
    ] "Disabled"
  ]

main :: IO ()
main = Iced.run [] "Button style" 0 update view
