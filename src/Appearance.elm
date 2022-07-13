module Appearance exposing
    ( Appearance
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
import Gender exposing (Gender(..))
import Random


type Appearance
    = Appearance String


generator : Random.Generator Appearance
generator =
    Random.map Appearance
        (Random.uniform "Distinctive jewelery: earrings, necklace, circlet, bracelets"
            [ "Piercings"
            , "Flamboyont or outlandish clothes"
            , "Formal, clean clothes"
            , "Ragged, dirty clothes"
            , "Pronounced scar"
            , "Missing teeth"
            , "Missing fingers"
            , "Unusual eye color (or two different colors)"
            , "Tattoos"
            , "Birthmark"
            , "Unusual skin color"
            , "Bald"
            , "Braided beard or hair"
            , "Unusual hair color"
            , "Nervous eye twitch"
            , "Distinctive nose"
            , "Distinctive posture"
            , "Exceptionally beautiful"
            , "Exceptionally ugly"
            ]
        )


toString : Appearance -> String
toString (Appearance str) =
    str



-- JSON


codec : Codec Appearance
codec =
    Codec.custom
        (\build (Appearance string) ->
            build string
        )
        |> Codec.variant1 "Appearance" Appearance Codec.string
        |> Codec.buildCustom
