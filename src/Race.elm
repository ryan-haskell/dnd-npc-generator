module Race exposing
    ( Race
    , codec
    , generator
    , toAgeRanges
    , toAlignmentWeights
    , toDimensionBounds
    , toNameGenerators
    , toString
    )

import Alignment
import Codec exposing (Codec)
import Random


type Race
    = Dwarf DwarfSubrace
    | Elf ElfSubrace
    | Halfling HalflingSubrace
    | Human HumanSubrace
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


type HumanSubrace
    = Calishite
    | Chondathan
    | Damaran


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

        Human _ ->
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
            Random.map Human
                humanSubraceGenerator

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

        Dragonborn ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }

        Elf _ ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }

        Halfling _ ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }

        Human _ ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }

        Gnome _ ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }

        HalfElf ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }

        HalfOrc ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }

        Tiefling ->
            { social = { lawful = 1, neutral = 1, chaotic = 1 }
            , moral = { good = 1, neutral = 1, evil = 1 }
            }


toAgeRanges : Race -> { min : Int, max : Int }
toAgeRanges race =
    case race of
        Dwarf _ ->
            { min = 50, max = 350 }

        Elf _ ->
            { min = 100, max = 750 }

        Halfling _ ->
            { min = 20, max = 150 }

        Human _ ->
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



-- |> Debug.log "TODO"


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

        Elf _ ->
            { maleFirstNameGenerator = Random.uniform "Adran" [ "Aelar", "Aramil" ]
            , femaleFirstNameGenerator = Random.uniform "Adrie" [ "Althaea" ]
            , lastNameGenerator = Random.uniform "Amakiir" [ "Amastacia", "Galanodel" ]
            }

        Halfling _ ->
            { maleFirstNameGenerator = Random.uniform "Alton" [ "Cade", "Eldon", "Finnan", "Garret" ]
            , femaleFirstNameGenerator = Random.uniform "Andry" [ "Bree", "Cora", "Euphemia", "Kithri" ]
            , lastNameGenerator = Random.uniform "Brushgather" [ "Goodbarrel", "High-hill", "Tealeaf" ]
            }

        Human Calishite ->
            { maleFirstNameGenerator = Random.uniform "Aseir" [ "Bardeid", "Haseid", "Khemed", "Mehmen" ]
            , femaleFirstNameGenerator = Random.uniform "Atala" [ "Ceidil", "Hama", "Jasmal", "Meilil", "Seipora", "Yasheira" ]
            , lastNameGenerator = Random.uniform "Basha" [ "Dumein", "Jassan", "Khalid", "Mostana" ]
            }

        Human Chondathan ->
            { maleFirstNameGenerator = Random.uniform "Darvin" [ "Dorn", "Evendur", "Gorstag", "Grim" ]
            , femaleFirstNameGenerator = Random.uniform "Arveene" [ "Esvele", "Jhessail", "Kerri", "Lureene" ]
            , lastNameGenerator = Random.uniform "Amblecrown" [ "Buckman", "Dundragon", "Evenwood" ]
            }

        Human Damaran ->
            { maleFirstNameGenerator = Random.uniform "Bor" [ "Fodel", "Glar", "Grigor", "Igan" ]
            , femaleFirstNameGenerator = Random.uniform "Alethra" [ "Kara", "Katernin", "Mara", "Natali" ]
            , lastNameGenerator = Random.uniform "Bersk" [ "Chernin", "Dotsk", "Kulenov", "Marsk" ]
            }

        Dragonborn ->
            { maleFirstNameGenerator = Random.uniform "Arjhan" [ "Balasar", "Donaar", "Ghesh", "Kriv" ]
            , femaleFirstNameGenerator = Random.uniform "Akra" [ "Biri", "Daar", "Harann", "Jheri" ]
            , lastNameGenerator = Random.uniform "Clethtinthiallor" [ "Daardendrian", "Fenkenkabradon", "Kimbatuul", "Myastan" ]
            }

        Gnome _ ->
            { maleFirstNameGenerator = Random.uniform "Alston" [ "Boddynock", "Dimble", "Erky", "Fonkin" ]
            , femaleFirstNameGenerator = Random.uniform "Bimpnottin" [ "Caramip", "Donella", "Ellyjobell", "Loopmottin" ]
            , lastNameGenerator = Random.uniform "Beren" [ "Daergel", "Folkor", "Garrick", "Nackle" ]
            }

        HalfElf ->
            { maleFirstNameGenerator = Random.uniform "Darvin" [ "Dorn", "Evendur", "Gorstag", "Grim" ]
            , femaleFirstNameGenerator = Random.uniform "Arveene" [ "Esvele", "Jhessail", "Kerri", "Lureene" ]
            , lastNameGenerator = Random.uniform "Amblecrown" [ "Buckman", "Dundragon", "Evenwood" ]
            }

        HalfOrc ->
            { maleFirstNameGenerator = Random.uniform "Dench" [ "Feng", "Gell", "Holg", "Imsh" ]
            , femaleFirstNameGenerator = Random.uniform "Baggi" [ "Emen", "Kansif", "Myev", "Ovak" ]
            , lastNameGenerator = Random.uniform "Ront" [ "Shump", "Volen", "Sutha" ]
            }

        Tiefling ->
            { maleFirstNameGenerator = Random.uniform "Akmenos" [ "Barakas", "Damakos", "Ekemon", "Iados" ]
            , femaleFirstNameGenerator = Random.uniform "Akta" [ "Bryseis", "Criella", "Ea", "Kallista" ]
            , lastNameGenerator = Random.uniform "Carrion" [ "Despair", "Glory", "Nowhere" ]
            }


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


humanSubraceGenerator : Random.Generator HumanSubrace
humanSubraceGenerator =
    Random.uniform Calishite
        [ Chondathan
        , Damaran
        ]


gnomeSubRaceGenerator : Random.Generator GnomeSubrace
gnomeSubRaceGenerator =
    Random.uniform ForestGnome
        [ RockGnome
        ]



-- JSON


codec : Codec Race
codec =
    Codec.custom
        (\dwa elf halfl hum dra gno halfe halfo tie value ->
            case value of
                Dwarf subrace ->
                    dwa subrace

                Elf subrace ->
                    elf subrace

                Halfling subrace ->
                    halfl subrace

                Human subrace ->
                    hum subrace

                Dragonborn ->
                    dra

                Gnome subrace ->
                    gno subrace

                HalfElf ->
                    halfe

                HalfOrc ->
                    halfo

                Tiefling ->
                    tie
        )
        |> Codec.variant1 "Dwarf" Dwarf dwarfSubraceCodec
        |> Codec.variant1 "Elf" Elf elfSubraceCodec
        |> Codec.variant1 "Halfling" Halfling halflingSubraceCodec
        |> Codec.variant1 "Human" Human humanSubraceCodec
        |> Codec.variant0 "Dragonborn" Dragonborn
        |> Codec.variant1 "Gnome" Gnome gnomeSubraceCodec
        |> Codec.variant0 "HalfElf" HalfElf
        |> Codec.variant0 "HalfOrc" HalfOrc
        |> Codec.variant0 "Tiefling" Tiefling
        |> Codec.buildCustom


dwarfSubraceCodec : Codec DwarfSubrace
dwarfSubraceCodec =
    Codec.custom
        (\hillDwarf mountainDwarf value ->
            case value of
                HillDwarf ->
                    hillDwarf

                MountainDwarf ->
                    mountainDwarf
        )
        |> Codec.variant0 "HillDwarf" HillDwarf
        |> Codec.variant0 "MountainDwarf" MountainDwarf
        |> Codec.buildCustom


elfSubraceCodec : Codec ElfSubrace
elfSubraceCodec =
    Codec.custom
        (\high wood dark value ->
            case value of
                HighElf ->
                    high

                WoodElf ->
                    wood

                DarkElf ->
                    dark
        )
        |> Codec.variant0 "HighElf" HighElf
        |> Codec.variant0 "WoodElf" WoodElf
        |> Codec.variant0 "DarkElf" DarkElf
        |> Codec.buildCustom


halflingSubraceCodec : Codec HalflingSubrace
halflingSubraceCodec =
    Codec.custom
        (\lightfoothalfling stouthalfling value ->
            case value of
                LightfootHalfling ->
                    lightfoothalfling

                StoutHalfling ->
                    stouthalfling
        )
        |> Codec.variant0 "LighfootHalfling" LightfootHalfling
        |> Codec.variant0 "StoutHalfing" StoutHalfling
        |> Codec.buildCustom


humanSubraceCodec : Codec HumanSubrace
humanSubraceCodec =
    Codec.custom
        (\calishite chondathan damaran value ->
            case value of
                Calishite ->
                    calishite

                Chondathan ->
                    chondathan

                Damaran ->
                    damaran
        )
        |> Codec.variant0 "Calishite" Calishite
        |> Codec.variant0 "Chondathan" Chondathan
        |> Codec.variant0 "Damaran" Damaran
        |> Codec.buildCustom


gnomeSubraceCodec : Codec GnomeSubrace
gnomeSubraceCodec =
    Codec.custom
        (\forestGnome rockGnome value ->
            case value of
                ForestGnome ->
                    forestGnome

                RockGnome ->
                    rockGnome
        )
        |> Codec.variant0 "ForestGnome" ForestGnome
        |> Codec.variant0 "RockGnome" RockGnome
        |> Codec.buildCustom
