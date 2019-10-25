module Types exposing (Dimensions, Position, Positionable)


type alias Positionable thing =
    { thing | x : Float, y : Float }


type alias Position =
    { x : Float, y : Float }


type alias Dimensions =
    { width : Float, height : Float }
