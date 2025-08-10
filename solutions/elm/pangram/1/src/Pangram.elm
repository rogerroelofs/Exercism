module Pangram exposing (isPangram)


isPangram : String -> Bool
isPangram sentence =
    let
        alphabet =
            "abcdefghijklmnopqrstuvwxyz"

        lowercasedSentence =
            String.toLower sentence
    in
    List.all (\letter -> String.contains letter lowercasedSentence) (String.toList alphabet |> List.map String.fromChar)
