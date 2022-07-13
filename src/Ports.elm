port module Ports exposing
    ( loadCharacters
    , saveCharacter
    )

import Json.Encode


port saveCharacter : { id : Int, character : Json.Encode.Value } -> Cmd msg


port loadCharacters : (Json.Encode.Value -> msg) -> Sub msg
