module Name exposing
    ( Name
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
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



-- JSON


codec : Codec Name
codec =
    Codec.custom
        (\constructor (Name first last) ->
            constructor first last
        )
        |> Codec.variant2 "Name" Name Codec.string Codec.string
        |> Codec.buildCustom
