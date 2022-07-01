module Flaw exposing
    ( Flaw
    , generator
    , toString
    )

import Random


type Flaw
    = Flaw String


generator : Random.Generator Flaw
generator =
    Random.map Flaw
        (Random.uniform "Forbidden love or susceptibility to romance"
            [ "Enjoys dedacent pleasures"
            , "Arrogance"
            , "Envies another creature's possesions or station"
            , "Overpowering greed"
            , "Prone to rage"
            , "Has a poweful enemy"
            , "Specific phobia"
            , "Shameful or scandalous history"
            , "Secret crime or misdeed"
            , "Possession of forbidden love"
            , "Foolhardy bravery"
            ]
        )


toString : Flaw -> String
toString (Flaw str) =
    str
