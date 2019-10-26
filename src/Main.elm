module Main exposing (main)

import Basics exposing (floor)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyUp, onResize)
import Color exposing (hsla)
import Color.Convert exposing (colorToCssHsla)
import Debug exposing (log, toString)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import List
import RoundedRectangle exposing (roundedRectCenteredAt)
import String exposing (concat)
import Styles.Main exposing (debug, world)
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Types exposing (Dimensions, Position, Positionable)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type State
    = WaitingForNodeToBePlaced
    | EditingNodeText Node
    | WaitingForFirstAction


type alias Node =
    Positionable { id : Int, text : String }


type alias Model =
    { state : State
    , lastNodeId : Int
    , nodes : List Node
    , nodeBeingPositioned : Maybe Node
    , viewbox : Dimensions
    , mouse : Maybe Position
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = WaitingForFirstAction
      , lastNodeId = -1
      , nodes = []
      , nodeBeingPositioned = Nothing
      , viewbox = Dimensions 0 0
      , mouse = Nothing
      }
    , Task.perform AdjustViewboxFromInitial getViewport
    )



-- UPDATE


type Msg
    = TrackMouseMovementAt Position
    | WaitForNodeToBePlaced
    | PlaceNodeAt Position
    | AdjustViewboxFromInitial Viewport
    | AdjustViewboxFromResize Int Int
    | ReturnToWaitingForFirstAction
    | DoNothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TrackMouseMovementAt pos ->
            ( { model | mouse = Just pos }, Cmd.none )

        WaitForNodeToBePlaced ->
            ( { model | state = WaitingForNodeToBePlaced }
            , Cmd.none
            )

        PlaceNodeAt pos ->
            let
                nextNodeId =
                    model.lastNodeId + 1

                newNode =
                    newNodeAt pos nextNodeId
            in
            ( { model
                | nodes = model.nodes ++ [ newNode ]
                , lastNodeId = nextNodeId
                , state = EditingNodeText newNode
              }
            , Cmd.none
            )

        AdjustViewboxFromInitial viewport ->
            let
                { width, height } =
                    viewport.viewport
            in
            ( { model | viewbox = Dimensions width height }, Cmd.none )

        AdjustViewboxFromResize width height ->
            ( { model | viewbox = Dimensions (toFloat width) (toFloat height) }
            , Cmd.none
            )

        ReturnToWaitingForFirstAction ->
            ( { model | state = WaitingForFirstAction }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


newNodeAt : Position -> Int -> Node
newNodeAt pos id =
    { id = id, x = pos.x, y = pos.y, text = "" }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch (commonSubscriptions ++ specificSubscriptions model)


commonSubscriptions : List (Sub Msg)
commonSubscriptions =
    [ onResize AdjustViewboxFromResize ]


specificSubscriptions : Model -> List (Sub Msg)
specificSubscriptions model =
    case model.state of
        WaitingForNodeToBePlaced ->
            [ onKeyUp (D.map mapKeyDecoderWhenWaitingForNodeToBePlaced keyDecoder) ]

        WaitingForFirstAction ->
            [ onKeyUp (D.map mapKeyDecoderForWaitingForFirstAction keyDecoder) ]

        _ ->
            []


mapKeyDecoderForWaitingForFirstAction : String -> Msg
mapKeyDecoderForWaitingForFirstAction key =
    let
        _ =
            log "(waiting for first action) Key: " key
    in
    if key == "n" then
        WaitForNodeToBePlaced

    else
        DoNothing


mapKeyDecoderWhenWaitingForNodeToBePlaced : String -> Msg
mapKeyDecoderWhenWaitingForNodeToBePlaced key =
    let
        _ =
            log "(capturing mouse move) Key: " key
    in
    if key == "Escape" then
        ReturnToWaitingForFirstAction

    else
        DoNothing


keyDecoder : D.Decoder String
keyDecoder =
    D.field "key" D.string


mouseDecoder : D.Decoder Position
mouseDecoder =
    D.map2 Position (D.field "clientX" D.float) (D.field "clientY" D.float)



-- VIEW


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div [ HA.class debug ] [ H.text ("State: " ++ modelName model) ]
        , S.svg
            (svgAttributes model)
            (nodeElementToBePlaced model ++ placedNodeElements model)
        ]


svgAttributes : Model -> List (S.Attribute Msg)
svgAttributes model =
    constantSvgAttributes model
        ++ svgAttributesWhileWaitingForNodeToBePlaced model


constantSvgAttributes : Model -> List (S.Attribute Msg)
constantSvgAttributes model =
    [ SA.class world
    , SA.viewBox
        (joinIntsWith
            " "
            [ 0, 0, floor model.viewbox.width, floor model.viewbox.height ]
        )
    , SA.preserveAspectRatio "none"
    , SE.on "mousemove" (D.map TrackMouseMovementAt mouseDecoder)
    ]


svgAttributesWhileWaitingForNodeToBePlaced : Model -> List (S.Attribute Msg)
svgAttributesWhileWaitingForNodeToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            [ SE.on "click" (D.map PlaceNodeAt mouseDecoder) ]

        _ ->
            []


nodeElementToBePlaced : Model -> List (H.Html Msg)
nodeElementToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            case model.mouse of
                Just pos ->
                    [ nodeElement pos [ SA.stroke (colorToCssHsla (nodeBorderColor 0.6)) ] ]

                Nothing ->
                    []

        _ ->
            []


placedNodeElements : Model -> List (S.Svg msg)
placedNodeElements model =
    List.map placedNodeElement model.nodes


placedNodeElement : Node -> S.Svg msg
placedNodeElement node =
    nodeElement node
        [ SA.stroke (colorToCssHsla (nodeBorderColor 1))
        , SA.fill (colorToCssHsla nodeFillColor)
        ]


nodeElement : Positionable thing -> List (S.Attribute msg) -> S.Svg msg
nodeElement position attrs =
    roundedRectCenteredAt position (Dimensions 210 90) 5 attrs


modelName : Model -> String
modelName model =
    case model.state of
        WaitingForNodeToBePlaced ->
            "Waiting for node to be placed"

        EditingNodeText node ->
            "Editing text for node " ++ String.fromInt node.id

        WaitingForFirstAction ->
            "Waiting for first action"



--- UTILITIES


nodeBorderColor alpha =
    hsla (degrees 228) 0.57 0.68 alpha


nodeFillColor =
    hsla (degrees 228) 0.56 0.91 1


joinIntsWith : String -> List Int -> String
joinIntsWith separator ints =
    String.join separator (List.map String.fromInt ints)
