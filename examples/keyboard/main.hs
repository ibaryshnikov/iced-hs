{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Keyboard
import Iced.Keyboard.PhysicalKey (KeyCode)
import Iced.Subscription qualified as Subscription
import Iced.Widget

data Model = Model { message :: Maybe Message }

data Message = Pressed KeyCode | Released KeyCode

update :: Model -> Message -> Model
update model message = model { message = Just message }

view :: Model -> Element
view model = container [centerX Fill, centerY Fill] $
  text [size 20] $ label model.message

label :: Maybe Message -> String
label (Just message) = "Last action: " ++ showAction message
label Nothing = ""

showAction :: Message -> String
showAction (Pressed key) = "pressed " ++ show key
showAction (Released key) = "released " ++ show key

subscriptionFn :: Model -> IO (Subscription Message)
subscriptionFn _model = Subscription.batch [
    onKeyPress Pressed,
    onKeyRelease Released
  ]

main :: IO ()
main = Iced.run [subscription subscriptionFn] "Keyboard" model update view
  where model = Model { message = Nothing }
