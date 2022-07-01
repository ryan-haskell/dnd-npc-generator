module Talent exposing
    ( Talent
    , generator
    , toString
    )

import Random


type Talent
    = Talent String


generator : Random.Generator Talent
generator =
    Random.map Talent
        (Random.uniform "Plays a musical instrument"
            [ "Speaks several languages fluently"
            , "Unbelievably lucky"
            , "Perfect memory"
            , "Great with animals"
            , "Great with children"
            , "Great at solving puzzles"
            , "Great at one game"
            , "Great at impersonations"
            , "Draws beautifully"
            , "Paints beautifully"
            , "Sings beautifully"
            , "Drinks everyone under the table"
            , "Expert carpenter"
            , "Expert cook"
            , "Expert dart thrower and rock skipper"
            , "Expert juggler"
            , "Skilled actor and master of disguise"
            , "Skilled dancer"
            , "Knows thieves' cant"
            ]
        )


toString : Talent -> String
toString (Talent str) =
    str
