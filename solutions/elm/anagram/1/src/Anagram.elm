module Anagram exposing (detect)


detect : String -> List String -> List String
detect word candidates =
    List.filter (isAnagram word) candidates

isAnagram : String -> String -> Bool
isAnagram word candidate =
    String.toLower word /= String.toLower candidate && sort word == sort candidate

sort : String -> String
sort =
    String.toLower >> String.toList >> List.sort >> String.fromList
