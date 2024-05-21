{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Keyboard.Key
import Iced.Widget

data Model = Model { message :: Maybe Message }

data Message = Pressed Named | Released Named

update :: Model -> Message -> Model
update model message = model { message = Just message }

view :: Model -> Element
view model = container [centerX, centerY] $
  text [size 20] $ label model.message

label :: Maybe Message -> String
label (Just message) = "Last action: " ++ showAction message
label Nothing = ""

showAction :: Message -> String
showAction (Pressed key) = "pressed " ++ show key
showAction (Released key) = "released " ++ show key

main :: IO ()
main = Iced.run [] "Keyboard" model update view
  where model = Model { message = Nothing }
