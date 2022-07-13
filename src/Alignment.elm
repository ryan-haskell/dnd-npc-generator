module Alignment exposing
    ( Alignment
    , MoralWeights
    , SocialWeights
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
import Random


type Alignment
    = Alignment Internals


type alias Internals =
    { social : SocialAlignment
    , moral : MoralAlignment
    }


type MoralAlignment
    = Good
    | Neutral_Moral
    | Evil


type SocialAlignment
    = Lawful
    | Neutral_Social
    | Chaotic


toString : Alignment -> String
toString (Alignment { social, moral }) =
    case ( social, moral ) of
        ( Neutral_Social, Neutral_Moral ) ->
            "True Neutral"

        _ ->
            fromSocialAlignmentToString social ++ " " ++ fromMoralAlignmentToString moral


fromSocialAlignmentToString : SocialAlignment -> String
fromSocialAlignmentToString social =
    case social of
        Lawful ->
            "Lawful"

        Neutral_Social ->
            "Neutral"

        Chaotic ->
            "Chaotic"


fromMoralAlignmentToString : MoralAlignment -> String
fromMoralAlignmentToString moral =
    case moral of
        Good ->
            "Good"

        Neutral_Moral ->
            "Neutral"

        Evil ->
            "Evil"


generator : { social : SocialWeights, moral : MoralWeights } -> Random.Generator Alignment
generator weights =
    Random.map Alignment
        (Random.map2 Internals
            (socialAlignmentGenerator weights.social)
            (moralAlignmentGenerator weights.moral)
        )


type alias SocialWeights =
    { lawful : Float
    , neutral : Float
    , chaotic : Float
    }


type alias MoralWeights =
    { good : Float
    , neutral : Float
    , evil : Float
    }


socialAlignmentGenerator : SocialWeights -> Random.Generator SocialAlignment
socialAlignmentGenerator { lawful, neutral, chaotic } =
    Random.weighted ( lawful, Lawful )
        [ ( neutral, Neutral_Social )
        , ( chaotic, Chaotic )
        ]


moralAlignmentGenerator : MoralWeights -> Random.Generator MoralAlignment
moralAlignmentGenerator { good, neutral, evil } =
    Random.weighted ( good, Good )
        [ ( neutral, Neutral_Moral )
        , ( evil, Evil )
        ]



-- JSON


codec : Codec Alignment
codec =
    Codec.custom (\build (Alignment value) -> build value)
        |> Codec.variant1 "Alignment"
            Alignment
            (Codec.object Internals
                |> Codec.field "social" .social socialCodec
                |> Codec.field "moral" .moral moralCodec
                |> Codec.buildObject
            )
        |> Codec.buildCustom


socialCodec : Codec SocialAlignment
socialCodec =
    Codec.custom
        (\lawful neutral chaotic value ->
            case value of
                Lawful ->
                    lawful

                Neutral_Social ->
                    neutral

                Chaotic ->
                    chaotic
        )
        |> Codec.variant0 "Lawful" Lawful
        |> Codec.variant0 "Neutral_Social" Neutral_Social
        |> Codec.variant0 "Chaotic" Chaotic
        |> Codec.buildCustom


moralCodec : Codec MoralAlignment
moralCodec =
    Codec.custom
        (\good neutral evil value ->
            case value of
                Good ->
                    good

                Neutral_Moral ->
                    neutral

                Evil ->
                    evil
        )
        |> Codec.variant0 "Good" Good
        |> Codec.variant0 "Neutral_Social" Neutral_Moral
        |> Codec.variant0 "Evil" Evil
        |> Codec.buildCustom
