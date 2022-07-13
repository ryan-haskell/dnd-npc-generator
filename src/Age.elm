module Age exposing
    ( Age
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
import Random


type Age
    = Age Bounds Int


type alias Bounds =
    { min : Int
    , max : Int
    }


generator : { min : Int, max : Int } -> Random.Generator Age
generator ({ min, max } as bounds) =
    Random.map (Age bounds)
        (Random.int min max)


toAgeDescription : Age -> String
toAgeDescription (Age { min, max } age) =
    let
        lifeCompletionPercent : Float
        lifeCompletionPercent =
            toFloat (age - min) / toFloat (max - min)
    in
    if lifeCompletionPercent < 0.25 then
        "Young"

    else if lifeCompletionPercent > 0.75 then
        "Old"

    else
        "Adult"


toString : Age -> String
toString age =
    let
        (Age _ ageValue) =
            age
    in
    toAgeDescription age ++ " (" ++ String.fromInt ageValue ++ " years old)"


codec : Codec Age
codec =
    Codec.custom
        (\build (Age bounds value) ->
            build bounds value
        )
        |> Codec.variant2 "Age"
            Age
            (Codec.object Bounds
                |> Codec.field "min" .min Codec.int
                |> Codec.field "max" .max Codec.int
                |> Codec.buildObject
            )
            Codec.int
        |> Codec.buildCustom
