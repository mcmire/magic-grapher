module Node exposing
    ( Id
    , Node
    , decodeId
    , encodeId
    , placedAt
    , unplacedAt
    , updateContent
    )

import Json.Decode as D
import Json.Encode as E
import NodeContent exposing (NodeContent)
import Types exposing (Dimensions, Position, Range)


type alias Id =
    Int


type alias Node =
    { id : Id
    , pos : Position
    , dims : Dimensions
    , content : NodeContent
    , isSelected : Bool
    }


updateContent : (NodeContent -> NodeContent) -> Node -> Node
updateContent fn node =
    { node | content = fn node.content }


placedAt : Position -> Int -> Node
placedAt pos id =
    { id = id
    , pos = pos
    , dims = { width = 210.0, height = 90.0 }
    , content = NodeContent.init
    , isSelected = False
    }


unplacedAt : Position -> Node
unplacedAt pos =
    placedAt pos -1


decodeId : D.Decoder Id
decodeId =
    D.int


encodeId : Int -> E.Value
encodeId =
    E.int
