{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import Iced.Application as Application
import Iced.Widget
import Iced.Widget.Button
import Iced.Widget.Column
import Iced.Widget.Text

data Model = Model {
    value :: Int
}

data Message = Inc | Dec

initModel :: Model
initModel = Model {
  value = 0
}

update :: Model -> Message -> IO (Model)
update model message = do
  return $ case message of
    Inc -> model { value = model.value + 1 }
    Dec -> model { value = model.value - 1 }

view :: Model -> IO(Element)
view model = do
  button_inc <- button "Inc" Inc
  label <- text $ show model.value
  button_dec <- button "Dec" Dec
  column $ [button_inc, label, button_dec]

main :: IO ()
main = Application.run "Counter" initModel update view
