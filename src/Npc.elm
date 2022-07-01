module Npc exposing (Npc, generator, view)

import Age exposing (Age)
import Alignment exposing (Alignment)
import Class exposing (Class)
import Dimensions exposing (Dimensions)
import Gender exposing (Gender)
import Html exposing (Html)
import Name exposing (Name)
import Race exposing (Race)
import Random
import Random.Extra


type alias Npc =
    { seed : Int
    , gender : Gender
    , race : Race
    , class : Class
    , name : Name
    , alignment : Alignment
    , age : Age
    , dimensions : Dimensions
    }


view : Npc -> Html msg
view npc =
    let
        viewDetailList : List ( String, String ) -> Html msg
        viewDetailList items =
            Html.ul []
                (List.map viewDetailItem items)

        viewDetailItem : ( String, String ) -> Html msg
        viewDetailItem ( label, value ) =
            Html.li []
                [ Html.strong [] [ Html.text label ]
                , Html.span [] [ Html.text (": " ++ value) ]
                ]
    in
    Html.div []
        [ Html.h1 [] [ Html.text (Name.toString npc.name) ]
        , viewDetailList
            [ ( "Race", Race.toString npc.race )
            , ( "Class", Class.toString npc.class )
            , ( "Alignment", Alignment.toString npc.alignment )
            , ( "Age", Age.toString npc.age )
            , ( "Height", Dimensions.toHeightString npc.dimensions )
            , ( "Weight", Dimensions.toWeightString npc.dimensions )
            ]
        ]


generator : Int -> Random.Generator Npc
generator seed =
    let
        fromGender : Gender -> Random.Generator Npc
        fromGender gender =
            Race.generator
                |> Random.andThen (toNpc gender)

        toNpc : Gender -> Race -> Random.Generator Npc
        toNpc gender race =
            Random.constant (Npc seed gender race)
                |> Random.Extra.andMap Class.generator
                |> Random.Extra.andMap (Name.generator gender (Race.toNameGenerators race))
                |> Random.Extra.andMap (Alignment.generator (Race.toAlignmentWeights race))
                |> Random.Extra.andMap (Age.generator (Race.toAgeRanges race))
                |> Random.Extra.andMap (Dimensions.generator (Race.toDimensionBounds race))
    in
    Gender.generator
        |> Random.andThen fromGender
