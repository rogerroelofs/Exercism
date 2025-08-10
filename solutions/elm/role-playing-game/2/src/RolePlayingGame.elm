module RolePlayingGame exposing (Player, castSpell, introduce, revive)


type alias Player =
    { name : Maybe String
    , level : Int
    , health : Int
    , mana : Maybe Int
    }


introduce : Player -> String
introduce { name } =
    Maybe.withDefault "Mighty Magician" name


revive : Player -> Maybe Player
revive player =
    case ( player.health > 0, player.level >= 10 ) of
        ( True, _ ) ->
            Nothing

        ( False, True ) ->
            Just { player | health = 100, mana = Just 100 }

        ( False, False ) ->
            Just { player | health = 100 }


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case player.mana of
        Nothing ->
            ( { player | health = max (player.health - manaCost) 0 }, 0 )

        Just mana ->
            if mana < manaCost then
                ( player, 0 )

            else
                ( { player | mana = Just (mana - manaCost) }, manaCost * 2 )
