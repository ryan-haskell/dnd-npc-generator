module Race exposing (Race, generator, toAgeRanges, toAlignmentWeights, toDimensionBounds, toNameGenerators, toString)

import Alignment
import Random


type Race
    = Dwarf DwarfSubrace
    | Elf ElfSubrace
    | Halfling
    | Human
    | Dragonborn
    | Gnome
    | HalfElf
    | HalfOrc
    | Tiefling


toString : Race -> String
toString race =
    case race of
        Dwarf HillDwarf ->
            "Hill Dwarf"

        Dwarf MountainDwarf ->
            "Mountain Dwarf"

        Elf HighElf ->
            "High Elf"

        Elf WoodElf ->
            "Wood Elf"

        Elf DarkElf ->
            "Dark Elf (Drow)"

        Halfling ->
            "Halfling"

        Human ->
            "Human"

        Dragonborn ->
            "Dragonborn"

        Gnome ->
            "Gnome"

        HalfElf ->
            "Half-elf"

        HalfOrc ->
            "Half-orc"

        Tiefling ->
            "Tiefling"


type PrimaryRace
    = DwarfRace
    | ElfRace
    | HalflingRace
    | HumanRace
    | DragonbornRace
    | GnomeRace
    | HalfElfRace
    | HalfOrcRace
    | TieflingRace


type DwarfSubrace
    = HillDwarf
    | MountainDwarf


type ElfSubrace
    = HighElf
    | WoodElf
    | DarkElf


generator : Random.Generator Race
generator =
    primaryRaceGenerator
        |> Random.andThen toRaceGenerator


toRaceGenerator : PrimaryRace -> Random.Generator Race
toRaceGenerator primaryRace =
    case primaryRace of
        DwarfRace ->
            Random.map Dwarf
                dwarfSubRaceGenerator

        ElfRace ->
            Random.map Elf
                elfSubRaceGenerator

        HalflingRace ->
            Random.constant Halfling

        HumanRace ->
            Random.constant Human

        DragonbornRace ->
            Random.constant Dragonborn

        GnomeRace ->
            Random.constant Gnome

        HalfElfRace ->
            Random.constant HalfElf

        HalfOrcRace ->
            Random.constant HalfOrc

        TieflingRace ->
            Random.constant Tiefling


primaryRaceGenerator : Random.Generator PrimaryRace
primaryRaceGenerator =
    Random.uniform DwarfRace [] |> Debug.log "TODO"



-- [ ElfRace
-- , HalflingRace
-- , HumanRace
-- , DragonbornRace
-- , GnomeRace
-- , HalfElfRace
-- , HalfOrcRace
-- , TieflingRace
-- ]


toAlignmentWeights : Race -> { social : Alignment.SocialWeights, moral : Alignment.MoralWeights }
toAlignmentWeights race =
    case race of
        Dwarf _ ->
            { social = { lawful = 5, neutral = 2, chaotic = 1 }
            , moral = { good = 4, neutral = 2, evil = 1 }
            }

        _ ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }
                |> Debug.log "TODO"


toAgeRanges : Race -> { min : Int, max : Int }
toAgeRanges race =
    case race of
        Dwarf _ ->
            { min = 50, max = 350 }

        _ ->
            { min = 20, max = 100 } |> Debug.log "TODO"


toDimensionBounds : Race -> { heightInInches : ( Int, Int ), weightInPounds : ( Int, Int ) }
toDimensionBounds race =
    case race of
        Dwarf _ ->
            { heightInInches = ( 48, 60 )
            , weightInPounds = ( 125, 250 )
            }

        _ ->
            { heightInInches = ( 60, 78 )
            , weightInPounds = ( 125, 250 )
            }
                |> Debug.log "TODO"


toNameGenerators :
    Race
    ->
        { maleFirstNameGenerator : Random.Generator String
        , femaleFirstNameGenerator : Random.Generator String
        , lastNameGenerator : Random.Generator String
        }
toNameGenerators race =
    case race of
        Dwarf _ ->
            { maleFirstNameGenerator =
                Random.uniform "Adrik"
                    [ "Alberich"
                    , "Baern"
                    , "Barendd"
                    , "Brottor"
                    , "Bruenor"
                    , "Dain"
                    , "Darrak"
                    , "Delg"
                    , "Eberk"
                    , "Einkil"
                    , "Fargrim"
                    , "Flint"
                    , "Gardain"
                    , "Harbek"
                    , "Kildrak"
                    , "Morgran"
                    , "Orsik"
                    , "Oskar"
                    , "Rangrim"
                    , "Rurik"
                    , "Taklinn"
                    , "Thoradin"
                    , "Thorin"
                    , "Tordek"
                    , "Traubon"
                    , "Travok"
                    , "Ulfgar"
                    , "Veit"
                    , "Vondal"
                    ]
            , femaleFirstNameGenerator =
                Random.uniform "Amber"
                    [ "Artin"
                    , "Audhild"
                    , "Bardryn"
                    , "Dagnal"
                    , "Diesa"
                    , "Eldeth"
                    , "Falkrun"
                    , "Finellen"
                    , "Gunnnloda"
                    , "Gurdis"
                    , "Helja"
                    , "Hlin"
                    , "Kathra"
                    , "Kristryd"
                    , "Ilde"
                    , "Liftrasa"
                    , "Mardred"
                    , "Riswynn"
                    , "Sannl"
                    , "Torbera"
                    , "Torgga"
                    , "Vistra"
                    ]
            , lastNameGenerator =
                Random.uniform "Balderk"
                    [ "Battlehammer"
                    , "Brawnanvil"
                    , "Dankil"
                    , "Fireforge"
                    , "Frostbeard"
                    , "Gorunn"
                    , "Holderhek"
                    , "Ironfist"
                    , "Loderr"
                    , "Lutgehr"
                    , "Rumnaheim"
                    , "Strakeln"
                    , "Torunn"
                    , "Ungart"
                    ]
            }

        _ ->
            { maleFirstNameGenerator = Random.constant "???"
            , femaleFirstNameGenerator = Random.constant "???"
            , lastNameGenerator = Random.constant "???"
            }
                |> Debug.log "TODO"


dwarfSubRaceGenerator : Random.Generator DwarfSubrace
dwarfSubRaceGenerator =
    Random.uniform HillDwarf
        [ MountainDwarf
        ]


elfSubRaceGenerator : Random.Generator ElfSubrace
elfSubRaceGenerator =
    Random.uniform HighElf
        [ WoodElf
        , DarkElf
        ]
