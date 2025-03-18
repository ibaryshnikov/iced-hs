# Canvas widget

```haskell
canvas :: [Attribute] -> [Action] -> State -> Element

-- example
canvas [width Fill, height Fill] shapes model.state

shapes :: [Action]
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
drawImage
  :: Image.IntoHandle a
  => Float -- top left x
  -> Float -- top left y
  -> Float -- width
  -> Float -- height
  -> a -- handle
  -> Action

drawSvg
  :: Svg.IntoHandle a
  => Float -- top left x
  -> Float -- top left y
  -> Float -- width
  -> Float -- height
  -> a -- handle
  -> Action

-- shapes color
fill :: [Shape] -> Color -> Action

fillText :: Text -> Action

pushTransform :: Action

popTransform :: Action

rotate :: Float -> Action

scale :: Float -> Action

-- shapes color width
stroke :: [Shape] -> Color -> Float -> Action
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
