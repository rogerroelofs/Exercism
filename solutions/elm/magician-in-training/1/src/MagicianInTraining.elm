module MagicianInTraining exposing (..)

-- todo: import the Array module
import Array exposing (Array)

getCard : Int -> Array Int -> Maybe Int
getCard index deck =
    Array.get index deck


setCard : Int -> Int -> Array Int -> Array Int
setCard index newCard deck =
    Array.set index newCard deck


addCard : Int -> Array Int -> Array Int
addCard newCard deck =
    Array.push newCard deck


removeCard : Int -> Array Int -> Array Int
removeCard index deck =
    Array.append (Array.slice 0 index deck) (Array.slice (index + 1) (Array.length deck) deck)

evenCardCount : Array Int -> Int
evenCardCount deck =
    Array.filter isEven deck |> Array.length

isEven : Int -> Bool
isEven n = (modBy 2 n) == 0
