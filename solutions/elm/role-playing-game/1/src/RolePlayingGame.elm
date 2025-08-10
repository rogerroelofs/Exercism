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
    case (player.health > 0, player.level >= 10) of
        (True, _) -> Nothing
        (False, True) -> Just { player | health = 100, mana = Just 100 }
        (False, False) -> Just { player | health = 100 }


castSpell : Int -> Player -> ( Player, Int )
castSpell manaCost player =
    case (player.mana == Nothing, Maybe.withDefault 0 player.mana < manaCost) of
        (True, _) -> ({player | health = 
            if player.health > manaCost then 
                player.health - manaCost 
            else 0
            }, 0)
        (False, True) -> (player, 0)
        (False, False) -> ({player | mana = Just (Maybe.withDefault 0 player.mana - manaCost)}, manaCost * 2)
