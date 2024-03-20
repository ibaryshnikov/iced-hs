{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Application
import Widgets

data Model = Model {
    value:: Int
}

data Message = Inc | Dec

update:: Model -> Message -> Model
update model message = case message of
  Inc -> Model { value = model.value + 1 }
  Dec -> Model { value = model.value - 1 }

view:: Model -> IO(Element)
view model = do
  button_inc <- button "Inc" Inc
  label <- text $ "Counter value " ++ show model.value
  button_dec <- button "Dec" Dec
  column [button_inc, label, button_dec]

main = Application.run Model { value = 0 } update view
