module RoundedRectangle exposing (roundedRectCenteredAt)

import Basics exposing (toFloat)
import Svg as S
import Svg.Attributes as SA
import Types exposing (Dimensions, Positionable)


type BoxedNumber
    = BoxedInt Int
    | BoxedFloat Float


roundedRectCenteredAt : Positionable thing -> Dimensions -> Float -> List (S.Attribute msg) -> S.Svg msg
roundedRectCenteredAt { x, y } { width, height } radius attrs =
    let
        defaultAttrs =
            [ SA.d
                (String.join
                    " "
                    [ m (x - (width / 2) + radius) (y - (height / 2))
                    , h (width - (2 * radius))
                    , roundedCorner radius radius
                    , v (height - (2 * radius))
                    , roundedCorner -radius radius
                    , h -(width - (2 * radius))
                    , roundedCorner -radius -radius
                    , v -(height - (2 * radius))
                    , roundedCorner radius -radius
                    ]
                )
            , SA.fill "none"
            ]
    in
    S.path (attrs ++ defaultAttrs) []


roundedCorner : Float -> Float -> String
roundedCorner rx ry =
    a rx ry 0 False True rx ry


m : Float -> Float -> String
m x y =
    pathSegment "M" [ BoxedFloat x, BoxedFloat y ]


h : Float -> String
h p =
    pathSegment "h" [ BoxedFloat p ]


v : Float -> String
v p =
    pathSegment "v" [ BoxedFloat p ]


a : Float -> Float -> Float -> Bool -> Bool -> Float -> Float -> String
a rx ry xAxisRotation largeArcFlag sweepFlag x y =
    pathSegment "a"
        [ BoxedFloat rx
        , BoxedFloat ry
        , BoxedFloat xAxisRotation
        , BoxedInt (booleanToInt largeArcFlag)
        , BoxedInt (booleanToInt sweepFlag)
        , BoxedFloat x
        , BoxedFloat y
        ]


pathSegment : String -> List BoxedNumber -> String
pathSegment char numbers =
    String.join " " ([ char ] ++ List.map numberToString numbers)


numberToString : BoxedNumber -> String
numberToString n =
    case n of
        BoxedInt i ->
            String.fromInt i

        BoxedFloat f ->
            String.fromFloat f


booleanToInt : Bool -> Int
booleanToInt bool =
    case bool of
        True ->
            1

        False ->
            0
