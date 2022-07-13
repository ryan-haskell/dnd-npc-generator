module InteractionTrait exposing
    ( InteractionTrait
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
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



-- JSON


codec : Codec InteractionTrait
codec =
    Codec.custom
        (\build (InteractionTrait string) ->
            build string
        )
        |> Codec.variant1 "InteractionTrait" InteractionTrait Codec.string
        |> Codec.buildCustom
