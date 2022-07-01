module InteractionTrait exposing
    ( InteractionTrait
    , generator
    , toString
    )

import Random


type InteractionTrait
    = InteractionTrait String


generator : Random.Generator InteractionTrait
generator =
    Random.map InteractionTrait
        (Random.uniform "Argumentative"
            [ "Arrogant"
            , "Blustering"
            , "Rude"
            , "Curious"
            , "Friendly"
            , "Honest"
            , "Hot tempered"
            , "Irritable"
            , "Ponderous"
            , "Quiet"
            , "Suspicious"
            ]
        )


toString : InteractionTrait -> String
toString (InteractionTrait str) =
    str
