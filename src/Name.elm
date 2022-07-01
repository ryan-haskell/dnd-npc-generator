module Name exposing
    ( Name
    , generator
    , toString
    )

import Gender exposing (Gender)
import Random


type Name
    = Name String String


generator :
    Gender
    ->
        { maleFirstNameGenerator : Random.Generator String
        , femaleFirstNameGenerator : Random.Generator String
        , lastNameGenerator : Random.Generator String
        }
    -> Random.Generator Name
generator gender options =
    Random.map2 Name
        (case gender of
            Gender.Male ->
                options.maleFirstNameGenerator

            Gender.Female ->
                options.femaleFirstNameGenerator
        )
        options.lastNameGenerator


toString : Name -> String
toString (Name first last) =
    first ++ " " ++ last
