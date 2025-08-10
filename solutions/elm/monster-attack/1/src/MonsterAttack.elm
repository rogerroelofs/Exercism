module MonsterAttack exposing (..)


type alias MonsterDamage =
    String

stitch : String -> MonsterDamage -> Int -> MonsterDamage
stitch weapon monsterDamage strength =
    monsterDamage ++ "Attacked with " ++ weapon ++ " of strength " ++ String.fromInt strength ++ "."

attackWithSword1 : MonsterDamage -> Int -> MonsterDamage
attackWithSword1 monsterDamage = stitch "sword" monsterDamage


attackWithClaw1 : MonsterDamage -> Int -> MonsterDamage
attackWithClaw1 monsterDamage = stitch "claw" monsterDamage


attack1 : MonsterDamage -> MonsterDamage
attack1 monsterDamage =
    monsterDamage ++ attackWithSword1 "" 5 ++ attackWithClaw1 "" 1 ++ attackWithClaw1 "" 1 ++ attackWithSword1 "" 5


attackWithSword2 : Int -> MonsterDamage -> MonsterDamage
attackWithSword2 strength monsterDamage = stitch "sword" monsterDamage strength


attackWithClaw2 : Int -> MonsterDamage -> MonsterDamage
attackWithClaw2 strength monsterDamage = stitch "claw" monsterDamage strength


attack2 : MonsterDamage -> MonsterDamage
attack2 monsterDamage =
    monsterDamage
    |> attackWithSword2 5
    |> attackWithClaw2 1
    |> attackWithClaw2 1
    |> attackWithSword2 5


attack3 : MonsterDamage -> MonsterDamage
attack3 =
    attackWithSword2 5
    >> attackWithClaw2 1
    >> attackWithClaw2 1
    >> attackWithSword2 5
