module Class exposing
    ( Class
    , generator
    , toString
    )

import Random


type Class
    = Barbarian
    | Rogue
    | Fighter


generator : Random.Generator Class
generator =
    Random.uniform Barbarian
        [ Rogue
        , Fighter
        ]


toString : Class -> String
toString class =
    case class of
        Barbarian ->
            "Barbarian"

        Rogue ->
            "Rogue"

        Fighter ->
            "Fighter"
