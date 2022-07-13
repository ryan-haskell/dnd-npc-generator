module Pages.Home_ exposing (Model, Msg, page)

import Character exposing (Character)
import Dict exposing (Dict)
import ElmLand.Page exposing (Page)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Ports
import Random
import View exposing (View)


page : Page Model Msg
page =
    ElmLand.Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { seed : Int
    , character : Maybe Character
    , notes : Dict SectionId String
    }


init : ( Model, Cmd Msg )
init =
    ( { character = Nothing
      , seed = 0
      , notes = Dict.empty
      }
    , Random.generate ElmGeneratedSeed (Random.int 0 Random.maxInt)
    )



-- UPDATE


type Msg
    = ElmGeneratedSeed Int
    | JavaScriptSentCharacters Json.Encode.Value
    | UserUpdatedSeedInput String
    | UserSubmittedSeedInput
    | UserClickedRandom
    | UserUpdatedNotesField SectionId String
    | UserClickedSaveCharacter CharacterId Json.Encode.Value


type alias SectionId =
    String


type alias CharacterId =
    Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JavaScriptSentCharacters json ->
            case json |> Json.Decode.decodeValue (Json.Decode.dict Character.decoder) of
                Ok charactersDict ->
                    case Dict.values charactersDict of
                        [] ->
                            ( model, Cmd.none )

                        first :: _ ->
                            ( { model | character = Just first }, Cmd.none )

                Err reason ->
                    ( model, Cmd.none )

        UserClickedRandom ->
            ( model
            , Random.generate ElmGeneratedSeed (Random.int 0 Random.maxInt)
            )

        UserSubmittedSeedInput ->
            ( { model | character = Just (generateCharacterFromSeed model.seed) }
            , Cmd.none
            )

        UserUpdatedSeedInput seedStr ->
            case String.toInt seedStr of
                Just seedInt ->
                    ( { model | seed = seedInt }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ElmGeneratedSeed seed ->
            let
                character : Character
                character =
                    generateCharacterFromSeed seed
            in
            ( { model | seed = seed, character = Just character }
            , Cmd.none
            )

        UserUpdatedNotesField sectionId textAreaValue ->
            ( { model
                | notes = Dict.insert sectionId textAreaValue model.notes
              }
            , Cmd.none
            )

        UserClickedSaveCharacter characterId characterAsJson ->
            ( model
            , Ports.saveCharacter
                { id = characterId
                , character = characterAsJson
                }
            )


generateCharacterFromSeed : Int -> Character
generateCharacterFromSeed seed =
    Random.initialSeed seed
        |> Random.step (Character.generator seed)
        |> (\( character, _ ) -> character)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.loadCharacters JavaScriptSentCharacters



-- VIEW


view : Model -> View Msg
view model =
    { title = "Character Generator"
    , body =
        [ Html.div [ Html.Attributes.class "controls" ]
            [ Html.form [ Html.Events.onSubmit UserSubmittedSeedInput ]
                [ Html.input
                    [ Html.Events.onInput UserUpdatedSeedInput
                    , Html.Attributes.type_ "number"
                    , Html.Attributes.value (String.fromInt model.seed)
                    ]
                    []
                , Html.button [] [ Html.text "Generate" ]
                ]
            , Html.button [ Html.Events.onClick UserClickedRandom ] [ Html.text "Random" ]
            ]
        , case model.character of
            Just character ->
                Html.div [ Html.Attributes.class "container" ]
                    [ Character.view
                        { notes = model.notes
                        , onNotesFieldInput = UserUpdatedNotesField
                        , onSaveClicked = UserClickedSaveCharacter
                        }
                        character
                    ]

            Nothing ->
                Html.text ""
        ]
    }
