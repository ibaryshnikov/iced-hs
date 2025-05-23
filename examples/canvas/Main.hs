{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import System.Directory (doesFileExist)

import Iced
import Iced.Attribute
import Iced.Attribute.Alignment
import Iced.Attribute.Text
import Iced.Color
import Iced.Widget
import Iced.Widget.Canvas qualified as Canvas
import Iced.Widget.Canvas.Frame
import Iced.Widget.Canvas.Shape
import Iced.Widget.Canvas.Text

data Model = Model
  { state :: Canvas.State
  , filePrefix :: String
  }

data Message

update :: Message -> Model -> Model
update _message model = model

view :: Model -> Element
view model = canvas [width Fill, height Fill] actions model.state
 where
  actions = shapes imagePath svgPath
  prefix = model.filePrefix
  imagePath = prefix ++ "image/watch_3.png"
  svgPath = prefix ++ "canvas/simple.svg"

shapes :: String -> String -> [Action]
shapes imagePath svgPath =
  [ stroke
      [ moveTo 300 300
      , lineTo 300 350
      , lineTo 350 350
      , lineTo 400 300
      , lineTo 350 300
      , lineTo 350 250
      , lineTo 310 290
      , circle 500 500 50
      , circle 200 200 10
      , rectangle 30 30 50 50
      ]
      (rgb8 0 0 0)
      4
  , fill
      [ circle 700 700 50
      ]
      $ rgb8 150 200 50
  , pushTransform
  , fillText $ label "One" 540 180
  , rotate (-pi / 2)
  , fillText $ label "Two" (-250) 475
  , rotate (pi)
  , scale 1.5
  , fillText $ label "Three" 165 (-422)
  , popTransform
  , drawImage 50 250 150 150 imagePath
  , drawSvg 200 50 150 100 svgPath
  , stroke
      [ circle 300 500 70
      ]
      (rgb8 255 50 50)
      4
  , fill
      [ rectangle 500 200 100 100
      ]
      $ rgba8 255 0 0 0.4
  ]

label :: String -> Float -> Float -> Text
label content x y =
  Text
    { content = content
    , x = x
    , y = y
    , color = rgb8 150 200 50
    , size = 30
    , lineHeight = Absolute 30
    , horizontal = Center
    , vertical = Center
    , shaping = Advanced
    }

getFilePrefix :: String -> IO String
getFilePrefix path = do
  exists <- doesFileExist path
  pure $
    if exists
      then "../"
      else "examples/"

main :: IO ()
main = do
  filePrefix <- getFilePrefix "simple.svg"
  state <- Canvas.newState
  let model = Model{state = state, filePrefix = filePrefix}
  Iced.run [] "Canvas" model update view
