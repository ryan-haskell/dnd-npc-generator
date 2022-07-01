module Bond exposing
    ( Bond
    , generator
    , toString
    )

import Random


type Bond
    = Bond String


generator : Random.Generator Bond
generator =
    Random.map Bond
        (Random.uniform "Dedicated to fulfilling a personal life goal"
            [ "Protective of close family members"
            , "Protective of colleagues or compatriots"
            , "Loyal to a benefactor, patron, or employer"
            , "Captivated by a romantic interest"
            , "Drawn to a special place"
            , "Protective of a sentimental keepsake"
            , "Protective of a valuable possesion"
            , "Out for revenge"
            ]
        )


toString : Bond -> String
toString (Bond str) =
    str
