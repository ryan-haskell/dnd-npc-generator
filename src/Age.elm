module Age exposing
    ( Age
    , generator
    , toString
    )

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
