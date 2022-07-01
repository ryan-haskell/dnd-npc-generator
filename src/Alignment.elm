module Alignment exposing
    ( Alignment
    , MoralWeights
    , SocialWeights
    , generator
    , toString
    )

import Random


type Alignment
    = Alignment SocialAlignment MoralAlignment


type MoralAlignment
    = Good
    | Neutral_Moral
    | Evil


type SocialAlignment
    = Lawful
    | Neutral_Social
    | Chaotic


toString : Alignment -> String
toString (Alignment social moral) =
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
    Random.map2 Alignment
        (socialAlignmentGenerator weights.social)
        (moralAlignmentGenerator weights.moral)


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
