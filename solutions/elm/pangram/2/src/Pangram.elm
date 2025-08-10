module Pangram exposing (isPangram)


isPangram : String -> Bool
isPangram sentence =
    List.all (\letter -> String.contains letter (String.toLower sentence)) (String.toList "abcdefghijklmnopqrstuvwxyz" |> List.map String.fromChar)
