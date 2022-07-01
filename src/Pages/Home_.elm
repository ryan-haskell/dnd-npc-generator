module Pages.Home_ exposing (Model, Msg, page)

import Character exposing (Character)
import ElmLand.Page exposing (Page)
import Html exposing (Html)
import Html.Attributes
import Html.Events
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
    }


init : ( Model, Cmd Msg )
init =
    ( { character = Nothing, seed = 0 }
    , Random.generate ElmGeneratedSeed (Random.int 0 Random.maxInt)
    )



-- UPDATE


type Msg
    = ElmGeneratedSeed Int
    | UserUpdatedSeedInput String
    | UserSubmittedSeedInput
    | UserClickedRandom


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


generateCharacterFromSeed : Int -> Character
generateCharacterFromSeed seed =
    Random.initialSeed seed
        |> Random.step (Character.generator seed)
        |> (\( character, _ ) -> character)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



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
                Html.div [ Html.Attributes.class "container" ] [ Character.view character ]

            Nothing ->
                Html.text ""
        ]
    }
