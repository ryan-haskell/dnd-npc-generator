module Class exposing
    ( Class
    , generator
    , toHitDie
    , toString
    )

import Random


type Class
    = Barbarian
    | Bard
    | Cleric
    | Druid
    | Fighter
    | Monk
    | Paladin
    | Ranger
    | Rogue
    | Sorcerer
    | Warlock
    | Wizard


generator : Random.Generator Class
generator =
    Random.uniform Barbarian
        [ Bard
        , Cleric
        , Druid
        , Fighter
        , Monk
        , Paladin
        , Ranger
        , Rogue
        , Sorcerer
        , Warlock
        , Wizard
        ]


toString : Class -> String
toString class =
    case class of
        Barbarian ->
            "Barbarian"

        Bard ->
            "Bard"

        Cleric ->
            "Cleric"

        Druid ->
            "Druid"

        Fighter ->
            "Fighter"

        Monk ->
            "Monk"

        Paladin ->
            "Paladin"

        Ranger ->
            "Ranger"

        Rogue ->
            "Rogue"

        Sorcerer ->
            "Sorcerer"

        Warlock ->
            "Warlock"

        Wizard ->
            "Wizard"


toHitDie : Class -> String
toHitDie class =
    case class of
        Barbarian ->
            "d12"

        Bard ->
            "d8"

        Cleric ->
            "d8"

        Druid ->
            "d8"

        Fighter ->
            "d10"

        Monk ->
            "d8"

        Paladin ->
            "d10"

        Ranger ->
            "d10"

        Rogue ->
            "d10"

        Sorcerer ->
            "d6"

        Warlock ->
            "d8"

        Wizard ->
            "d6"
