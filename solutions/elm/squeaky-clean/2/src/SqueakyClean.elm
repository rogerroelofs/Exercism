module SqueakyClean exposing (clean, clean1, clean2, clean3, clean4)


clean1 : String -> String
clean1 str =
    String.replace " " "_" str

clean2 : String -> String
clean2 str =
    List.foldl 
        (\c -> String.replace c "[CTRL]")
        (clean1 str)
        ["\n", "\r", "\t"]


clean3 : String -> String
clean3 str =
    case String.split "-" (clean2 str) of
        [] ->
            ""
        first :: rest ->
            String.concat (first :: List.map cap rest)

cap : String -> String
cap str =
    case String.uncons str of
        Nothing ->
            str
        Just ( first, rest ) ->
            String.fromChar (Char.toUpper first) ++ rest


clean4 : String -> String
clean4 str =
    str |> clean3 |> String.filter (Char.isDigit >> not)


clean : String -> String
clean str =
     let
        notGreek c =
            Char.toCode c < Char.toCode 'α' || Char.toCode 'ω' < Char.toCode c
    in
    str |> clean4 |> String.filter notGreek