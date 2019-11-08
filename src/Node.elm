module Node exposing
    ( Node
    , placedAt
    , unplacedAt
    , updateContent
    )

import NodeContent exposing (Model)
import NodeId exposing (NodeId)
import Types exposing (Dimensions, Position, Range)


type alias Node =
    { id : NodeId
    , pos : Position
    , dims : Dimensions
    , content : NodeContent.Model
    , isSelected : Bool
    }


updateContent : (NodeContent.Model -> NodeContent.Model) -> Node -> Node
updateContent fn node =
    { node | content = fn node.content }


placedAt : Position -> NodeId -> Node
placedAt pos id =
    { id = id
    , pos = pos
    , dims = { width = 210.0, height = 90.0 }
    , content = NodeContent.init id
    , isSelected = False
    }


unplacedAt : Position -> Node
unplacedAt pos =
    placedAt pos -1
