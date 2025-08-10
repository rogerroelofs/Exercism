module Allergies exposing (Allergy(..), isAllergicTo, toList)
import Bitwise exposing (..)

type Allergy
    = Eggs
    | Peanuts
    | Shellfish
    | Strawberries
    | Tomatoes
    | Chocolate
    | Pollen
    | Cats

-- is there a way to map a type to an integer?
allergyToScore : List ( Allergy, Int )
allergyToScore = [ ( Eggs, 1 ), ( Peanuts, 2 ), ( Shellfish, 4 ), ( Strawberries, 8 ), ( Tomatoes, 16 ), ( Chocolate, 32 ), ( Pollen, 64 ), ( Cats, 128 ) ]

isAllergicTo : Allergy -> Int -> Bool
isAllergicTo allergy score =
    -- This function should return True if the person is allergic to the given allergen
    -- based on the score, and False otherwise.
    List.member allergy (toList score)


toList : Int -> List Allergy
toList score =
    List.map Tuple.first (List.filter (\x -> and score (Tuple.second x) == Tuple.second x) allergyToScore)