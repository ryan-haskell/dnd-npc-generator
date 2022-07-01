module Appearance exposing
    ( Appearance
    , generator
    , toString
    )

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
