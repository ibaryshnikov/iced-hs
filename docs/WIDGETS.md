# Widgets

List of all widgets

```haskell
import Iced.Widget
```

[button](#button) [text](#text)


## Button

```haskell
button :: [Attribute message] -> String -> Element

-- example
button [onPress Click] "Click me"
```


## Text

```haskell
text :: [Attribute] -> String -> Element

-- example
text [size 50] "Welcome"
```
