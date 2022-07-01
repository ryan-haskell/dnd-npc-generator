module Gender exposing
    ( Gender(..)
    , generator
    , toString
    )

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
