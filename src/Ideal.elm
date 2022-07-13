module Ideal exposing
    ( Ideal
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
import Random


type Ideal
    = Ideal String


generator : Random.Generator Ideal
generator =
    Random.map Ideal
        (Random.uniform "Beauty"
            -- GOOD
            [ "Charity"
            , "Greater good"
            , "Life"
            , "Respect"
            , "Self-sacrifice"

            -- EVIL
            , "Domination"
            , "Greed"
            , "Might"
            , "Pain"
            , "Retribution"
            , "Slaughter"

            -- LAWFUL
            , "Community"
            , "Fairness"
            , "Honor"
            , "Logic"
            , "Responsibility"
            , "Tradition"

            -- CHAOTIC
            , "Change"
            , "Creativity"
            , "Freedom"
            , "Independence"
            , "No limits"
            , "Whimsy"

            -- NEUTRAL
            , "Balance"
            , "Knowledge"
            , "Live and let live"
            , "Moderation"
            , "Neutrality"
            , "People"

            -- OTHER
            , "Aspiration"
            , "Discovery"
            , "Glory"
            , "Nation"
            , "Redemption"
            , "Self-knowledge"
            ]
        )


toString : Ideal -> String
toString (Ideal str) =
    str



-- JSON


codec : Codec Ideal
codec =
    Codec.custom
        (\build (Ideal string) ->
            build string
        )
        |> Codec.variant1 "Ideal" Ideal Codec.string
        |> Codec.buildCustom
