module TopScorers exposing (..)

import Dict exposing (Dict)
import TopScorersSupport exposing (PlayerName)
import Maybe exposing (withDefault)


updateGoalCountForPlayer : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
updateGoalCountForPlayer playerName playerGoalCounts =
    playerGoalCounts
        |> Dict.update playerName (\value -> Just (1 + (withDefault 0 value)))


aggregateScorers : List PlayerName -> Dict PlayerName Int
aggregateScorers playerNames =
    playerNames
        |> List.foldl updateGoalCountForPlayer Dict.empty


removeInsignificantPlayers : Int -> Dict PlayerName Int -> Dict PlayerName Int
removeInsignificantPlayers goalThreshold playerGoalCounts =
    playerGoalCounts
        |> Dict.filter (\_ value -> value >= goalThreshold)


resetPlayerGoalCount : PlayerName -> Dict PlayerName Int -> Dict PlayerName Int
resetPlayerGoalCount playerName playerGoalCounts =
    playerGoalCounts
        |> Dict.update playerName (always (Just 0))


formatPlayer : PlayerName -> Dict PlayerName Int -> String
formatPlayer playerName playerGoalCounts =
    playerGoalCounts
        |> Dict.get playerName
        |> withDefault 0
        |> String.fromInt
        |> String.append (playerName ++ ": ")


formatPlayers : Dict PlayerName Int -> String
formatPlayers players =
    players
        |> Dict.keys 
        |> List.map (\playerName -> formatPlayer playerName players)
        |> String.join(", ")


combineGames : Dict PlayerName Int -> Dict PlayerName Int -> Dict PlayerName Int
combineGames game1 game2 =
    Dict.merge
        (\k a -> Dict.insert k a)
        (\k a b -> Dict.insert k (a + b))
        (\k a -> Dict.insert k a)
        game1
        game2 
        Dict.empty
