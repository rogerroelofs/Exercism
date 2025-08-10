module Acronym exposing (abbreviate)

import String exposing (replace, words, join, toUpper, left)
import List exposing (map)

abbreviate : String -> String
abbreviate phrase =
    phrase
        |> replace "-" " "
        |> words
        |> map (left 1)
        |> join ""
        |> toUpper