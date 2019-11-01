module Node exposing
    ( Content
    , Id
    , Node
    , decodeId
    , encodeId
    , placedAt
    , unplacedAt
    , updateContent
    )

import Json.Decode as D
import Json.Encode as E
import Types exposing (Dimensions, Position, Range)


type alias Id =
    Int


type alias Content =
    { text : String
    , nextText : String
    , width : Float
    , height : Float
    , fontSize : Float
    , isBeingEdited : Bool
    , cursorIndex : Int
    , cursorPosition : Float
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
        , nextText = ""

        -- TODO: Nothing?
        , width = 0

        -- TODO: Nothing?
        , height = 0
        , fontSize = 16.0
        , isBeingEdited = False

        -- TODO: Nothing?
        , cursorIndex = 0

        -- TODO: Nothing?
        , cursorPosition = 0.0
        }
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
