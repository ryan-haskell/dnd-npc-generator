module Talent exposing
    ( Talent
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
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



-- JSON


codec : Codec Talent
codec =
    Codec.custom
        (\build (Talent string) ->
            build string
        )
        |> Codec.variant1 "Talent" Talent Codec.string
        |> Codec.buildCustom
