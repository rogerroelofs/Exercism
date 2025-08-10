module Luhn exposing (valid)


valid : String -> Bool
valid input =
    let
        digits =
            String.toList input
                |> List.map String.fromChar
                |> List.filterMap String.toInt
                |> List.reverse

        doubleEveryOther =
            List.indexedMap
                (\index digit ->
                    if remainderBy 2 index == 0 then
                        digit

                    else
                        digit * 2
                )
                digits

        subtractNine =
            List.map
                (\digit ->
                    if digit > 9 then
                        digit - 9

                    else
                        digit
                )
                doubleEveryOther

        sum =
            List.sum subtractNine

        isValid =
            remainderBy 10 sum == 0
    in

    if (String.length (String.trim input) <= 1) || String.any (\c -> not (Char.isDigit c || c == ' ')) input then
        False
    else
        isValid
