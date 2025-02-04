# Widgets

List of all widgets

```haskell
import Iced.Widget
```

[button](#button) [canvas](#canvas) [checkbox](#checkbox) [column](#column)
[comboBox](#combobox) [container](#container) [image](#image) [markdown](#markdown)
[pickList](#picklist) [progressBar](#progressbar) [radio](#radio)
[responsive](#responsive) [row](#row) [scrollable](#scrollable) [slider](#slider)
[space](#space) [text](#text) [textEditor](#texteditor) [textInput](#textinput)
[toggler](#toggler) [tooltip](#tooltip)


## Button

```haskell
button :: [Attribute message] -> String -> Element

-- attributes

-- emit the message when the button is clicked
onPress :: message -> Attribute message

-- if True emit the message when the button is clicked
onPressIf :: Bool -> message -> Attribute message

-- set the padding for all sides
padding :: Float -> Attribute message

-- set the padding as following:
-- first argument is top and bottom
-- second is left and right
padding2 :: Float -> Float -> Attribute message

-- set the padding as following:
-- top right bottom left
padding4 :: Float -> Float -> Float -> Float -> Attribute message

-- set the style of the button
style :: IntoStyle value => value -> Attribute message

-- set the width of the button
width :: Length -> Attribute message

-- set the height of the button
height :: Length -> Attribute message

-- example
button [onPress Click] "Click me"
```


## Canvas

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


## Checkbox

```haskell
checkbox :: [Attribute message] -> String -> Bool -> Element

-- example
checkbox [onToggle Toggled] "Checkbox" model.selected
-- assuming that
data Message = Toggled Bool
```


## Column

```haskell
column :: [Attribute] -> [Element] -> Element

-- example
column [alignX Center, spacing 10] [
  button [onPress Inc] "Increment",
  text [size 50] $ show value,
  button [onPress Dec] "Decrement"
]
```


## Text

```haskell
text :: [Attribute] -> String -> Element

-- example
text [size 50] "Welcome"
```
