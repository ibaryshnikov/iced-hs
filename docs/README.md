# Welcome to the documentation of iced-hs

This is a top-level documentation page


## Widgets

List of [widgets](./WIDGETS.md)

```haskell
import Iced.Widget
```


## Attributes

List of widget [attributes](ATTRIBUTES.md)

```haskell
import Iced.Attribute
```


## Themes

Detailed documentation for [themes](./THEMES.md)

```haskell
-- example
import Iced
import Iced.Theme

main :: IO ()
main = Iced.run [theme Oxocarbon] "App" model update view
```
