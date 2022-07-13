module Class exposing
    ( Class
    , codec
    , generator
    , toHitDie
    , toString
    )

import Codec exposing (Codec)
import Random


type Class
    = Barbarian
    | Bard
    | Cleric
    | Druid
    | Fighter
    | Monk
    | Paladin
    | Ranger
    | Rogue
    | Sorcerer
    | Warlock
    | Wizard


generator : Random.Generator Class
generator =
    Random.uniform Barbarian
        [ Bard
        , Cleric
        , Druid
        , Fighter
        , Monk
        , Paladin
        , Ranger
        , Rogue
        , Sorcerer
        , Warlock
        , Wizard
        ]


toString : Class -> String
toString class =
    case class of
        Barbarian ->
            "Barbarian"

        Bard ->
            "Bard"

        Cleric ->
            "Cleric"

        Druid ->
            "Druid"

        Fighter ->
            "Fighter"

        Monk ->
            "Monk"

        Paladin ->
            "Paladin"

        Ranger ->
            "Ranger"

        Rogue ->
            "Rogue"

        Sorcerer ->
            "Sorcerer"

        Warlock ->
            "Warlock"

        Wizard ->
            "Wizard"


toHitDie : Class -> String
toHitDie class =
    case class of
        Barbarian ->
            "d12"

        Bard ->
            "d8"

        Cleric ->
            "d8"

        Druid ->
            "d8"

        Fighter ->
            "d10"

        Monk ->
            "d8"

        Paladin ->
            "d10"

        Ranger ->
            "d10"

        Rogue ->
            "d10"

        Sorcerer ->
            "d6"

        Warlock ->
            "d8"

        Wizard ->
            "d6"



-- JSON


codec : Codec Class
codec =
    Codec.custom
        (\barb bard cler drui figh monk pala rang rogu sorc warl wiza value ->
            case value of
                Barbarian ->
                    barb

                Bard ->
                    bard

                Cleric ->
                    cler

                Druid ->
                    drui

                Fighter ->
                    figh

                Monk ->
                    monk

                Paladin ->
                    pala

                Ranger ->
                    rang

                Rogue ->
                    rogu

                Sorcerer ->
                    sorc

                Warlock ->
                    warl

                Wizard ->
                    wiza
        )
        |> Codec.variant0 "Barbarian" Barbarian
        |> Codec.variant0 "Bard" Bard
        |> Codec.variant0 "Cleric" Cleric
        |> Codec.variant0 "Druid" Druid
        |> Codec.variant0 "Fighter" Fighter
        |> Codec.variant0 "Monk" Monk
        |> Codec.variant0 "Paladin" Paladin
        |> Codec.variant0 "Ranger" Ranger
        |> Codec.variant0 "Rogue" Rogue
        |> Codec.variant0 "Sorcerer" Sorcerer
        |> Codec.variant0 "Warlock" Warlock
        |> Codec.variant0 "Wizard" Wizard
        |> Codec.buildCustom
