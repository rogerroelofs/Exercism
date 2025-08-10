module BinarySearch exposing (find)

import Array exposing (Array)


find : Int -> Array Int -> Maybe Int
find target xs =
    let
        go low high =
            if low > high then
                Nothing

            else
                let
                    mid =
                        (low + high) // 2

                    midValue =
                        xs |> Array.get mid |> Maybe.withDefault 0
                in
                if midValue == target then
                    Just mid

                else if midValue < target then
                    go (mid + 1) high

                else
                    go low (mid - 1)
    in
    go 0 (Array.length xs - 1)