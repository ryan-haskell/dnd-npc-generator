module Abilities exposing
    ( Abilities
    , generator
    , toString
    )

import Random


type Abilities
    = Abilities { high : Ability, low : Ability }


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
