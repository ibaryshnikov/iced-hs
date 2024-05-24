{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Iced
import Iced.Attribute
import Iced.Widget

data Model = Model { position :: Position }

data Message = ChangePosition

update :: Model -> Message -> Model
update model message = case message of
  ChangePosition -> model { position = position } where
    position = case model.position of
      Top -> Bottom
      Bottom -> LeftSide
      LeftSide -> RightSide
      RightSide -> FollowCursor
      FollowCursor -> Top

view :: Model -> Element
view model =
  container [centerX Fill, centerY Fill] $
  tooltip [gap 10] content (toText model.position) model.position
  where
    content = button [onPress ChangePosition] "Press to change position"

toText :: Position -> Element
toText position = text [] label where
  label = case position of
    FollowCursor -> "Follow Cursor"
    Top -> "Top"
    Bottom -> "Bottom"
    LeftSide -> "Left"
    RightSide -> "Right"

main :: IO ()
main = Iced.run [] "Tooltip" model update view
  where model = Model { position = FollowCursor }
