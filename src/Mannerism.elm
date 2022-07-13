module Mannerism exposing
    ( Mannerism
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
import Random


type Mannerism
    = Mannerism String


generator : Random.Generator Mannerism
generator =
    Random.map Mannerism
        (Random.uniform "Prone to singing, whistling, or humming quietly"
            [ "Speaks in rhyme or some other peculiar way"
            , "Particularly low or high voice"
            , "Slurs words, lisps or stutters"
            , "Enunciates overly clearly"
            , "Speaks loudly"
            , "Whispers"
            , "Uses flowery speech or long words"
            , "Frequently uses the wrong word"
            , "Uses colorful oaths and exclamations"
            , "Makes constant jokes or puns"
            , "Prone to predictions of doom"
            , "Fidgets"
            , "Squints"
            , "Stares into the distance"
            , "Chews something"
            , "Paces"
            , "Taps fingers"
            , "Bites fingernails"
            , "Twirls hair or tugs beard"
            ]
        )


toString : Mannerism -> String
toString (Mannerism str) =
    str



-- JSON


codec : Codec Mannerism
codec =
    Codec.custom
        (\build (Mannerism string) ->
            build string
        )
        |> Codec.variant1 "Mannerism" Mannerism Codec.string
        |> Codec.buildCustom
