module Race exposing (Race, generator, toAgeRanges, toAlignmentWeights, toDimensionBounds, toNameGenerators, toString)

import Alignment
import Random


type Race
    = Dwarf DwarfSubrace
    | Elf ElfSubrace
    | Halfling HalflingSubrace
    | Human
    | Dragonborn
    | Gnome GnomeSubrace
    | HalfElf
    | HalfOrc
    | Tiefling


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


type HalflingSubrace
    = LightfootHalfling
    | StoutHalfling


type GnomeSubrace
    = ForestGnome
    | RockGnome


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

        Halfling LightfootHalfling ->
            "Lightfoot Halfling"

        Halfling StoutHalfling ->
            "Stout Halfling"

        Human ->
            "Human"

        Dragonborn ->
            "Dragonborn"

        Gnome ForestGnome ->
            "Forest Gnome"

        Gnome RockGnome ->
            "Rock Gnome"

        HalfElf ->
            "Half-elf"

        HalfOrc ->
            "Half-orc"

        Tiefling ->
            "Tiefling"


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
            Random.map Halfling
                halflingSubRaceGenerator

        HumanRace ->
            Random.constant Human

        DragonbornRace ->
            Random.constant Dragonborn

        GnomeRace ->
            Random.map Gnome
                gnomeSubRaceGenerator

        HalfElfRace ->
            Random.constant HalfElf

        HalfOrcRace ->
            Random.constant HalfOrc

        TieflingRace ->
            Random.constant Tiefling


primaryRaceGenerator : Random.Generator PrimaryRace
primaryRaceGenerator =
    Random.uniform DwarfRace
        [ ElfRace
        , HalflingRace
        , HumanRace
        , DragonbornRace
        , GnomeRace
        , HalfElfRace
        , HalfOrcRace
        , TieflingRace
        ]


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

        Elf _ ->
            { min = 100, max = 750 }

        Halfling _ ->
            { min = 20, max = 150 }

        Human ->
            { min = 18, max = 100 }

        Dragonborn ->
            { min = 15, max = 80 }

        Gnome _ ->
            { min = 40, max = 450 }

        HalfElf ->
            { min = 20, max = 200 }

        HalfOrc ->
            { min = 14, max = 75 }

        Tiefling ->
            { min = 18, max = 120 }


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


halflingSubRaceGenerator : Random.Generator HalflingSubrace
halflingSubRaceGenerator =
    Random.uniform LightfootHalfling
        [ StoutHalfling
        ]


gnomeSubRaceGenerator : Random.Generator GnomeSubrace
gnomeSubRaceGenerator =
    Random.uniform ForestGnome
        [ RockGnome
        ]
