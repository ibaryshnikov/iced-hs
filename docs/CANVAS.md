# Canvas widget

```haskell
canvas :: [Attribute] -> [FrameAction] -> State -> Element

-- example
canvas [width Fill, height Fill] shapes model.state

shapes :: [FrameAction]
shapes = [
    fill [
      circle 700 700 50
    ] $ rgb8 150 200 50,
    stroke [
      circle 300 500 70
    ] (rgb8 255 50 50) 4
  ]
```


## List of possible actions

```haskell
-- shapes color
fill :: [Shape] -> Color -> FrameAction

fillText :: Text -> FrameAction

pushTransform :: FrameAction

popTransform :: FrameAction

rotate :: Float -> FrameAction

scale :: Float -> FrameAction

-- shapes color width
stroke :: [Shape] -> Color -> Float -> FrameAction
```

## List of shapes

```haskell
-- x y radius
circle :: Float -> Float -> Float -> Shape

-- x y
lineTo :: Float -> Float -> Shape

-- x y
moveTo :: Float -> Float -> Shape

-- topLeftX topLeftY width height
rectangle :: Float -> Float -> Float -> Float -> Shape
```
