module ValentinesDay exposing (..)


type Approval
    = Yes
    | No
    | Maybe


type Cuisine
    = Korean
    | Turkish


type Genre
    = Crime
    | Horror
    | Romance
    | Thriller


type Activity
    = BoardGame
    | Chill
    | Movie Genre
    | Restaurant Cuisine


rateActivity : Activity -> Approval
rateActivity activity =
    case activity of
        BoardGame ->
            No

        Chill ->
            No

        Movie m ->
            case m of
                Romance ->
                    Yes

                _ ->
                    No

        Restaurant r ->
            case r of
                Korean ->
                    Yes

                Turkish ->
                    Maybe
