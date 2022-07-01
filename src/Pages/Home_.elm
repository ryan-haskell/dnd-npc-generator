module Pages.Home_ exposing (Model, Msg, page)

import ElmLand.Page exposing (Page)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Npc exposing (Npc)
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
    , npc : Maybe Npc
    }


init : ( Model, Cmd Msg )
init =
    ( { npc = Nothing, seed = 0 }
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
            ( { model | npc = Just (generateNpcFromSeed model.seed) }
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
                npc : Npc
                npc =
                    generateNpcFromSeed seed
            in
            ( { model | seed = seed, npc = Just npc }
            , Cmd.none
            )


generateNpcFromSeed : Int -> Npc
generateNpcFromSeed seed =
    Random.initialSeed seed
        |> Random.step (Npc.generator seed)
        |> (\( npc, _ ) -> npc)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , body =
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
        , case model.npc of
            Just npc ->
                Npc.view npc

            Nothing ->
                Html.text ""
        ]
    }
