{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget
import Iced.Widget.Tooltip qualified as Tooltip

data Model = Model { position :: Tooltip.Position }

data Message = ChangePosition

update :: Model -> Message -> Model
update model message = case message of
  ChangePosition -> model { position = position } where
    position = case model.position of
      Tooltip.Top -> Tooltip.Bottom
      Tooltip.Bottom -> Tooltip.Left
      Tooltip.Left -> Tooltip.Right
      Tooltip.Right -> Tooltip.FollowCursor
      Tooltip.FollowCursor -> Tooltip.Top

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  tooltip [gap 10] content (toText model.position) model.position
  where
    content = button [onPress ChangePosition] "Press to change position"

toText :: Tooltip.Position -> Element
toText position = text [] label where
  label = case position of
    Tooltip.FollowCursor -> "Follow Cursor"
    Tooltip.Top -> "Top"
    Tooltip.Bottom -> "Bottom"
    Tooltip.Left -> "Left"
    Tooltip.Right -> "Right"

main :: IO ()
main = Iced.run [] "Tooltip" model update view
  where model = Model { position = Tooltip.FollowCursor }
