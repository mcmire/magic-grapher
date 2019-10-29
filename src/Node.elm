module Node exposing
    ( Content
    , Id
    , Node
    , decodeId
    , placedAt
    , unplacedAt
    , updateContent
    )

import Json.Decode as D
import Types exposing (Dimensions, Position)


type alias Id =
    Int


type alias Content =
    { text : String
    , width : Float
    , height : Float
    , fontSize : Float
    , isBeingEdited : Bool
    }


type alias Node =
    { id : Id
    , pos : Position
    , dims : Dimensions
    , content : Content
    , isSelected : Bool
    }


updateContent : (Content -> Content) -> Node -> Node
updateContent fn node =
    { node | content = fn node.content }


placedAt : Position -> Int -> Node
placedAt pos id =
    { id = id
    , pos = pos
    , dims = { width = 210.0, height = 90.0 }
    , content =
        { text = ""
        , width = 0
        , height = 0
        , fontSize = 16.0
        , isBeingEdited = False
        }
    , isSelected = False
    }


unplacedAt : Position -> Node
unplacedAt pos =
    placedAt pos -1


decodeId : D.Decoder Id
decodeId =
    D.int
