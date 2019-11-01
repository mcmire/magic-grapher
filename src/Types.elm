module Types exposing (Dimensions, Position, Range)


type alias Position =
    { x : Float, y : Float }


type alias Dimensions =
    { width : Float, height : Float }


type alias Range =
    { start : Int, end : Int }
