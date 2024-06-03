module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Color
import Iced.Theme
import Iced.Widget
import Iced.Widget.Button qualified as Button

data Message = Click

update :: () -> Message -> ()
update value Click = value

firstStyle :: [Button.StyleAttribute]
firstStyle = [
    border (rgb8 0 0 0) 2 8,
    textColor (rgb8 255 255 255)
  ]

secondStyle :: [Button.StatusAttribute]
secondStyle = [
    active [background (rgb8 192 100 0)],
    hovered [background (rgb8 200 108 8)],
    pressed [background (rgb8 185 93 0)],
    disabled [background (rgb8 130 70 0)]
  ]

thirdStyle :: ([Button.StyleAttribute], [Button.StatusAttribute])
thirdStyle = ([
    border (rgb8 0 0 0) 1 4
  ], [
    active [textColor (rgb8 192 100 0)],
    hovered [textColor (rgb8 240 70 0)],
    pressed [textColor (rgb8 185 93 0)],
    disabled [textColor (rgb8 130 70 0)]
  ])

styleFn :: Button.Status -> [Button.StyleAttribute]
styleFn status = base ++ case status of
  Button.Active -> [background (rgb8 38 53 93)]
  Button.Hovered -> [background (rgb8 48 63 103)]
  Button.Pressed -> [background (rgb8 33 48 88)]
  Button.Disabled -> [background (rgb8 130 130 130)]
  where
    base = [
        border (rgb8 0 0 0) 1 4,
        textColor (rgb8 230 230 230)
      ]

view :: () -> Element
view _value =
  center [] $
  column [spacing 10, alignItems Start] [
    text [] "Default for current theme:",
    row [spacing 10] [
      button [onPress Click] "Active",
      button [] "Disabled"
    ],
    text [] "Basic styles for current theme:",
    row [spacing 10, alignItems Center] [
      button [style Button.Primary, onPress Click] "Active",
      button [style Button.Primary] "Disabled",
      text [] "Primary"
    ],
    row [spacing 10, alignItems Center] [
      button [style Button.Secondary, onPress Click] "Active",
      button [style Button.Secondary] "Disabled",
      text [] "Secondary"
    ],
    row [spacing 10, alignItems Center] [
      button [style Button.Success, onPress Click] "Active",
      button [style Button.Success] "Disabled",
      text [] "Success"
    ],
    row [spacing 10, alignItems Center] [
      button [style Button.Danger, onPress Click] "Active",
      button [style Button.Danger] "Disabled",
      text [] "Danger"
    ],
    row [spacing 10, alignItems Center] [
      button [style Button.Text, onPress Click] "Active",
      button [style Button.Text] "Disabled",
      text [] "Text"
    ],
    text [] "Styled with lists:",
    row [spacing 10] [
      button [style firstStyle, onPress Click] "Active",
      button [style firstStyle] "Disabled"
    ],
    row [spacing 10] [
      button [style secondStyle, onPress Click] "Active",
      button [style secondStyle] "Disabled"
    ],
    row [spacing 10] [
      button [style thirdStyle, onPress Click] "Active",
      button [style thirdStyle] "Disabled"
    ],
    text [] "Styled with function:",
    row [spacing 10] [
      button [style styleFn, onPress Click] "Active",
      button [style styleFn] "Disabled"
    ]
  ]

main :: IO ()
main = Iced.run [theme KanagawaLotus] "Button styles" () update view
