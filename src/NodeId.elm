module NodeId exposing (NodeId, decode, encode)

import Json.Decode as D
import Json.Encode as E


type alias NodeId =
    Int


decode : D.Decoder NodeId
decode =
    D.int


encode : Int -> E.Value
encode =
    E.int
