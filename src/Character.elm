module Character exposing
    ( Character
    , decoder
    , generator
    , view
    )

import Abilities exposing (Abilities)
import Age exposing (Age)
import Alignment exposing (Alignment)
import Appearance exposing (Appearance)
import Bond exposing (Bond)
import Class exposing (Class)
import Codec exposing (Codec)
import Dict exposing (Dict)
import Dimensions exposing (Dimensions)
import Flaw exposing (Flaw)
import Gender exposing (Gender)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Ideal exposing (Ideal)
import InteractionTrait exposing (InteractionTrait)
import Json.Decode
import Json.Encode
import Mannerism exposing (Mannerism)
import Name exposing (Name)
import Race exposing (Race)
import Random
import Random.Char exposing (char)
import Random.Extra
import Talent exposing (Talent)


type alias SectionId =
    String


type alias Character =
    { seed : Int
    , gender : Gender
    , race : Race
    , class : Class
    , name : Name
    , alignment : Alignment
    , age : Age
    , dimensions : Dimensions
    , extras : Extras
    }


type alias Extras =
    { appearance : Appearance
    , abilities : Abilities
    , talent : Talent
    , mannerism : Mannerism
    , interactionTrait : InteractionTrait
    , ideal : Ideal
    , bond : Bond
    , flaw : Flaw
    }


type alias CharacterId =
    Int


view :
    { onNotesFieldInput : SectionId -> String -> msg
    , notes : Dict SectionId String
    , onSaveClicked : CharacterId -> Json.Encode.Value -> msg
    }
    -> Character
    -> Html msg
view options character =
    let
        viewDetailList : List ( String, String ) -> Html msg
        viewDetailList items =
            Html.div []
                (List.map viewDetailItem items)

        viewDetailItem : ( String, String ) -> Html msg
        viewDetailItem ( label, value ) =
            Html.div []
                [ Html.strong [] [ Html.text label ]
                , Html.span [] [ Html.text (": " ++ value) ]
                ]

        viewSections : List ( SectionId, String, String ) -> Html msg
        viewSections sections =
            Html.div []
                (List.map viewSection sections)

        viewSection : ( SectionId, String, String ) -> Html msg
        viewSection ( id, header, content ) =
            Html.section []
                [ Html.h3 [] [ Html.text header ]
                , Html.p [] [ Html.text content ]
                , Html.textarea
                    [ Html.Events.onInput (options.onNotesFieldInput id)
                    , Attr.value (Dict.get id options.notes |> Maybe.withDefault "")
                    , Attr.disabled True
                    , Attr.placeholder "Notes coming soon!"

                    -- , Attr.placeholder ("Notes for " ++ String.toLower header ++ "...")
                    ]
                    []
                ]
    in
    Html.div []
        [ Html.div [ Attr.class "row gap-32" ]
            [ Html.div []
                [ Html.h1 [] [ Html.text (Name.toString character.name) ]
                , Html.button
                    [ Html.Events.onClick (options.onSaveClicked character.seed (toJson character))
                    ]
                    [ Html.text "Save" ]
                , viewDetailList
                    [ ( "Gender", Gender.toString character.gender )
                    , ( "Race & Class", String.join " " [ Race.toString character.race, Class.toString character.class ] )
                    , ( "Alignment", Alignment.toString character.alignment )
                    , ( "Age", Age.toString character.age )
                    , ( "Height", Dimensions.toHeightString character.dimensions )
                    , ( "Weight", Dimensions.toWeightString character.dimensions )
                    ]
                ]
            ]
        , Html.div [ Attr.class "row gap-32" ]
            [ viewSections
                [ ( "appearance", "Appearance", Appearance.toString character.extras.appearance )
                , ( "abilities", "Abilities", Abilities.toString character.extras.abilities )
                , ( "talent", "Talent", Talent.toString character.extras.talent )
                , ( "mannerism", "Mannerism", Mannerism.toString character.extras.mannerism )
                ]
            , viewSections
                [ ( "interactions", "Interactions", InteractionTrait.toString character.extras.interactionTrait )
                , ( "ideal", "Ideal", Ideal.toString character.extras.ideal )
                , ( "bond", "Bond", Bond.toString character.extras.bond )
                , ( "flaw", "Flaw or secret", Flaw.toString character.extras.flaw )
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
                |> Random.Extra.andMap extrasGenerator

        extrasGenerator : Random.Generator Extras
        extrasGenerator =
            Random.constant Extras
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



-- JSON


toJson : Character -> Json.Encode.Value
toJson character =
    Codec.encoder codec character


decoder : Json.Decode.Decoder Character
decoder =
    Codec.decoder codec


codec : Codec Character
codec =
    Codec.object Character
        |> Codec.field "seed" .seed Codec.int
        |> Codec.field "gender" .gender Gender.codec
        |> Codec.field "race" .race Race.codec
        |> Codec.field "class" .class Class.codec
        |> Codec.field "name" .name Name.codec
        |> Codec.field "alignment" .alignment Alignment.codec
        |> Codec.field "age" .age Age.codec
        |> Codec.field "dimensions" .dimensions Dimensions.codec
        |> Codec.field "extras" .extras extrasCodec
        |> Codec.buildObject


extrasCodec : Codec Extras
extrasCodec =
    Codec.object Extras
        |> Codec.field "appearance" .appearance Appearance.codec
        |> Codec.field "abilities" .abilities Abilities.codec
        |> Codec.field "talent" .talent Talent.codec
        |> Codec.field "mannerism" .mannerism Mannerism.codec
        |> Codec.field "interactionTrait" .interactionTrait InteractionTrait.codec
        |> Codec.field "ideal" .ideal Ideal.codec
        |> Codec.field "bond" .bond Bond.codec
        |> Codec.field "flaw" .flaw Flaw.codec
        |> Codec.buildObject
