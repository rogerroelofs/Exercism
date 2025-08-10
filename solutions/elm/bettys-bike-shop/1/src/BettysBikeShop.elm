module BettysBikeShop exposing (penceToPounds, poundsToString)

import String


penceToPounds : Float -> Float
penceToPounds pence =
    pence / 100


poundsToString : Float -> String
poundsToString pounds =
    String.concat [ "£", String.fromFloat pounds ]
