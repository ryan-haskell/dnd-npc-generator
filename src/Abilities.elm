module Abilities exposing
    ( Abilities
    , codec
    , generator
    , toString
    )

import Codec exposing (Codec)
import Random


type Abilities
    = Abilities Internals


type alias Internals =
    { high : Ability, low : Ability }


type Ability
    = Strength
    | Dexterity
    | Constitution
    | Intelligence
    | Wisdom
    | Charisma


generator : Random.Generator Abilities
generator =
    Random.map2 (\high low -> Abilities { low = low, high = high })
        abilityGenerator
        abilityGenerator


abilityGenerator : Random.Generator Ability
abilityGenerator =
    Random.uniform Strength
        [ Dexterity
        , Constitution
        , Intelligence
        , Wisdom
        , Charisma
        ]


toString : Abilities -> String
toString (Abilities { high, low }) =
    "High {{high}}, low {{low}}"
        |> String.replace "{{high}}" (fromAbilityToString high)
        |> String.replace "{{low}}" (fromAbilityToString low)


fromAbilityToString : Ability -> String
fromAbilityToString ability =
    case ability of
        Strength ->
            "Strength"

        Dexterity ->
            "Dexterity"

        Constitution ->
            "Constitution"

        Intelligence ->
            "Intelligence"

        Wisdom ->
            "Wisdom"

        Charisma ->
            "Charisma"



-- JSON


codec : Codec Abilities
codec =
    Codec.custom
        (\build (Abilities string) ->
            build string
        )
        |> Codec.variant1 "Abilities"
            Abilities
            (Codec.object Internals
                |> Codec.field "high" .high abilityCodec
                |> Codec.field "low" .low abilityCodec
                |> Codec.buildObject
            )
        |> Codec.buildCustom


abilityCodec : Codec Ability
abilityCodec =
    Codec.custom
        (\str dex con int wis cha value ->
            case value of
                Strength ->
                    str

                Dexterity ->
                    dex

                Constitution ->
                    con

                Intelligence ->
                    int

                Wisdom ->
                    wis

                Charisma ->
                    cha
        )
        |> Codec.variant0 "Strength" Strength
        |> Codec.variant0 "Dexterity" Dexterity
        |> Codec.variant0 "Constitution" Constitution
        |> Codec.variant0 "Intelligence" Intelligence
        |> Codec.variant0 "Wisdom" Wisdom
        |> Codec.variant0 "Charisma" Charisma
        |> Codec.buildCustom
