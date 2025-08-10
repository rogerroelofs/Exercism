module Go exposing (..)

import GoSupport exposing (..)
import Result exposing (andThen)


applyRules : Game -> Rule -> NonValidatingRule -> Rule -> Rule -> Game
applyRules game oneStonePerPointRule captureRule libertyRule koRule =
    case
        game
            |> koRule
            |> andThen libertyRule
            |> andThen oneStonePerPointRule
    of
        Ok valid ->
            valid
                |> captureRule
                |> changePlayer

        Err error ->
            { game | error = error }
