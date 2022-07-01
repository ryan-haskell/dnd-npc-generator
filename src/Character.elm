module Character exposing (Character, generator, view)

import Abilities exposing (Abilities)
import Age exposing (Age)
import Alignment exposing (Alignment)
import Appearance exposing (Appearance)
import Bond exposing (Bond)
import Class exposing (Class)
import Dimensions exposing (Dimensions)
import Flaw exposing (Flaw)
import Gender exposing (Gender)
import Html exposing (Html)
import Html.Attributes
import Ideal exposing (Ideal)
import InteractionTrait exposing (InteractionTrait)
import Mannerism exposing (Mannerism)
import Name exposing (Name)
import Race exposing (Race)
import Random
import Random.Extra
import Talent exposing (Talent)


type alias Character =
    { seed : Int
    , gender : Gender
    , race : Race
    , class : Class
    , name : Name
    , alignment : Alignment
    , age : Age
    , dimensions : Dimensions
    , npcStuff : NpcStuff
    }


type alias NpcStuff =
    { appearance : Appearance
    , abilities : Abilities
    , talent : Talent
    , mannerism : Mannerism
    , interactionTrait : InteractionTrait
    , ideal : Ideal
    , bond : Bond
    , flaw : Flaw
    }


view : Character -> Html msg
view character =
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

        viewSections : List ( String, String ) -> Html msg
        viewSections sections =
            Html.div []
                (List.map viewSection sections)

        viewSection : ( String, String ) -> Html msg
        viewSection ( header, content ) =
            Html.section []
                [ Html.h3 [] [ Html.text header ]
                , Html.p [] [ Html.text content ]
                ]
    in
    Html.div []
        [ Html.h1 [] [ Html.text (Name.toString character.name) ]
        , viewDetailList
            [ ( "Race", Race.toString character.race )
            , ( "Class", Class.toString character.class )
            , ( "Alignment", Alignment.toString character.alignment )
            , ( "Age", Age.toString character.age )
            , ( "Height", Dimensions.toHeightString character.dimensions )
            , ( "Weight", Dimensions.toWeightString character.dimensions )
            ]
        , Html.div [ Html.Attributes.class "row gap-32" ]
            [ viewSections
                [ ( "Appearance", Appearance.toString character.npcStuff.appearance )
                , ( "Abilities", Abilities.toString character.npcStuff.abilities )
                , ( "Talent", Talent.toString character.npcStuff.talent )
                , ( "Mannerism", Mannerism.toString character.npcStuff.mannerism )
                ]
            , viewSections
                [ ( "Interactions", InteractionTrait.toString character.npcStuff.interactionTrait )
                , ( "Ideal", Ideal.toString character.npcStuff.ideal )
                , ( "Bond", Bond.toString character.npcStuff.bond )
                , ( "Flaw or secret", Flaw.toString character.npcStuff.flaw )
                ]
            ]
        ]


generator : Int -> Random.Generator Character
generator seed =
    let
        fromGender : Gender -> Random.Generator Character
        fromGender gender =
            Race.generator
                |> Random.andThen (toCharacter gender)

        toCharacter : Gender -> Race -> Random.Generator Character
        toCharacter gender race =
            Random.constant (Character seed gender race)
                |> Random.Extra.andMap Class.generator
                |> Random.Extra.andMap (Name.generator gender (Race.toNameGenerators race))
                |> Random.Extra.andMap (Alignment.generator (Race.toAlignmentWeights race))
                |> Random.Extra.andMap (Age.generator (Race.toAgeRanges race))
                |> Random.Extra.andMap (Dimensions.generator (Race.toDimensionBounds race))
                |> Random.Extra.andMap npcStuffGenerator

        npcStuffGenerator : Random.Generator NpcStuff
        npcStuffGenerator =
            Random.constant NpcStuff
                |> Random.Extra.andMap Appearance.generator
                |> Random.Extra.andMap Abilities.generator
                |> Random.Extra.andMap Talent.generator
                |> Random.Extra.andMap Mannerism.generator
                |> Random.Extra.andMap InteractionTrait.generator
                |> Random.Extra.andMap Ideal.generator
                |> Random.Extra.andMap Bond.generator
                |> Random.Extra.andMap Flaw.generator
    in
    Gender.generator
        |> Random.andThen fromGender
