module Dimensions exposing
    ( Dimensions
    , generator
    , toHeightString
    , toWeightString
    )

import Random


type Dimensions
    = Dimensions HeightInInches WeightInPounds


type alias HeightInInches =
    Int


type alias WeightInPounds =
    Int


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
    Random.map2 Dimensions
        (Random.int minH maxH)
        (Random.int minW maxW)


toHeightString : Dimensions -> String
toHeightString (Dimensions heightInInches _) =
    String.fromInt (heightInInches // 12)
        ++ "' "
        ++ String.fromInt (modBy 12 heightInInches)
        ++ "\""


toWeightString : Dimensions -> String
toWeightString (Dimensions _ weightInPounds) =
    String.fromInt weightInPounds ++ " lbs"
