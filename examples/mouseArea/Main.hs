module Main where

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Theme
import Iced.Widget
import Iced.Widget.Container (BasicStyle (..))

data Message
  = DoubleClick
  | Enter
  | Exit
  | MiddlePress
  | MiddleRelease
  | Move Float Float
  | Press
  | Release
  | RightPress
  | RightRelease
  deriving Show

type Model = Maybe Message

update :: Message -> Model -> Model
update message _model = Just message

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
    column
      [alignX Center, spacing 10]
      [ text [size 30] "Last action:"
      , text [size 30] $ label model
      , mouseArea
          [ onDoubleClick DoubleClick
          , onEnter Enter
          , onExit Exit
          , onMiddlePress MiddlePress
          , onMiddleRelease MiddleRelease
          , onMove Move
          , onPress Press
          , onRelease Release
          , onRightPress RightPress
          , onRightRelease RightRelease
          ]
          content
      ]

label :: Maybe Message -> String
label Nothing = ""
label (Just message) = show message

content :: Element
content = container [width (Fixed 500), height (Fixed 500), style RoundedBox] $ text [] ""

main :: IO ()
main = Iced.run [theme KanagawaLotus] "MouseArea" Nothing update view
