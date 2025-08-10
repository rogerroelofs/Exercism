module ReverseString exposing (reverse)


reverse : String -> String
reverse str = 
    str
    |> String.foldr (\c acc -> acc ++ (String.fromChar c)) ""