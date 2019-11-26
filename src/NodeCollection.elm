module NodeCollection exposing
    ( NodeCollection
    , anyBeingEdited
    , empty
    , get
    , insert
    , update
    , updateNodeContentFor
    , values
    )

import Dict exposing (Dict)
import Node exposing (Node)
import NodeContent exposing (Model)
import NodeId exposing (NodeId)
import Types exposing (Position)


type alias NodeCollection =
    { entries : Dict NodeId Node
    , nextNodeId : NodeId
    }


empty : NodeCollection
empty =
    { entries = Dict.empty, nextNodeId = 0 }


insert : Position -> Bool -> NodeCollection -> ( NodeCollection, Node )
insert pos isSelected coll =
    let
        nextNodeId =
            coll.nextNodeId + 1

        newPlacedNode =
            Node.placedAt pos coll.nextNodeId

        newNode =
            { newPlacedNode | isSelected = isSelected }

        newCollection =
            { coll
                | entries = Dict.insert newNode.id newNode coll.entries
                , nextNodeId = newNode.id
            }
    in
    ( newCollection, newNode )


get : NodeId -> NodeCollection -> Maybe Node
get nodeId coll =
    Dict.get nodeId coll.entries


update : NodeId -> (Node -> Node) -> NodeCollection -> NodeCollection
update nodeId fn coll =
    let
        newEntries =
            Dict.update nodeId (Maybe.map fn) coll.entries
    in
    { coll | entries = newEntries }


updateNodeContentFor :
    NodeId
    -> (NodeContent.Model -> NodeContent.Model)
    -> NodeCollection
    -> NodeCollection
updateNodeContentFor nodeId fn coll =
    update nodeId (Node.updateContent fn) coll


values : NodeCollection -> List Node
values coll =
    Dict.values coll.entries


anyBeingEdited : NodeCollection -> Bool
anyBeingEdited coll =
    List.any (\node -> node.content.isBeingEdited) (values coll)
