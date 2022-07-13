module Gender exposing
    ( Gender(..)
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
import Json.Decode
import Json.Encode
import Random


type Gender
    = Male
    | Female


generator : Random.Generator Gender
generator =
    Random.uniform Male [ Female ]


toString : Gender -> String
toString gender =
    case gender of
        Male ->
            "Male"

        Female ->
            "Female"



-- JSON


codec : Codec Gender
codec =
    Codec.custom
        (\male female value ->
            case value of
                Male ->
                    male

                Female ->
                    female
        )
        |> Codec.variant0 "Male" Male
        |> Codec.variant0 "Female" Female
        |> Codec.buildCustom
