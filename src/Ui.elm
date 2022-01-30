module Ui exposing
  ( cell
  )

import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input

cell : List ( Element msg ) -> Element msg
cell contents =
  Element.column
    [ Element.Font.alignLeft
    , Element.Border.color <| Element.rgb255 0 0 0
    , Element.Border.width 1
    ]
    contents