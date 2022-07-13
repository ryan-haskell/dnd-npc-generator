module Dimensions exposing
    ( Dimensions
    , codec
    , generator
    , toHeightString
    , toWeightString
    )

import Codec exposing (Codec)
import Random


type Dimensions
    = Dimensions Internals


type alias Internals =
    { heightInInches : Int, weightInPounds : Int }


generator :
    { heightInInches : ( Int, Int )
    , weightInPounds : ( Int, Int )
    }
    -> Random.Generator Dimensions
generator bounds =
    let
        ( minH, maxH ) =
            bounds.heightInInches

        ( minW, maxW ) =
            bounds.weightInPounds
    in
    Random.map Dimensions
        (Random.map2 Internals
            (Random.int minH maxH)
            (Random.int minW maxW)
        )


toHeightString : Dimensions -> String
toHeightString (Dimensions { heightInInches }) =
    String.fromInt (heightInInches // 12)
        ++ "' "
        ++ String.fromInt (modBy 12 heightInInches)
        ++ "\""


toWeightString : Dimensions -> String
toWeightString (Dimensions { weightInPounds }) =
    String.fromInt weightInPounds ++ " lbs"



-- JSON


codec : Codec Dimensions
codec =
    Codec.custom
        (\build (Dimensions internals) ->
            build internals
        )
        |> Codec.variant1 "Dimensions"
            Dimensions
            (Codec.object Internals
                |> Codec.field "heightInInches" .heightInInches Codec.int
                |> Codec.field "weightInPounds" .weightInPounds Codec.int
                |> Codec.buildObject
            )
        |> Codec.buildCustom
