module Main exposing (main)

import Basics exposing (floor)
import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onKeyUp, onResize)
import Color exposing (hsla)
import Color.Convert exposing (colorToCssHsla)
import Debouncer.Messages as Debouncer
    exposing
        ( Debouncer
        , provideInput
        , throttle
        , toDebouncer
        )
import Debug exposing (log, toString)
import Html as H
import Html.Attributes as HA
import Json.Decode as D
import List
import RoundedRectangle exposing (roundedRect)
import String exposing (concat)
import Styles.Main exposing (debug, world)
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Task
import Types exposing (Dimensions, Position)


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
    { id : Int
    , pos : Position
    , dims : Dimensions
    , isSelected : Bool
    , text : String
    }


type alias Model =
    { state : State
    , lastNodeId : Int
    , nodes : List Node
    , nodeBeingPositioned : Maybe Node
    , viewbox : Dimensions
    , mouse : { pos : Maybe Position, cursor : String }
    , debouncer : Debouncer Msg
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = WaitingForFirstAction
      , lastNodeId = -1
      , nodes = []
      , nodeBeingPositioned = Nothing
      , viewbox = Dimensions 0 0
      , mouse = { pos = Nothing, cursor = "normal" }
      , debouncer = toDebouncer (throttle 250)
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
    | DebounceMsg (Debouncer.Msg Msg)
    | DoNothing


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = DebounceMsg
    , getDebouncer = .debouncer
    , setDebouncer = \debouncer model -> { model | debouncer = debouncer }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TrackMouseMovementAt pos ->
            let
                mouse =
                    model.mouse
            in
            ( { model | mouse = { mouse | pos = Just pos } }, Cmd.none )

        WaitForNodeToBePlaced ->
            ( { model | state = WaitingForNodeToBePlaced }
            , Cmd.none
            )

        PlaceNodeAt pos ->
            let
                nextNodeId =
                    model.lastNodeId + 1

                newNode =
                    placedNodeAt pos nextNodeId

                selectedNewNode =
                    { newNode | isSelected = True }
            in
            ( { model
                | nodes = model.nodes ++ [ selectedNewNode ]
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

        DebounceMsg subMsg ->
            Debouncer.update update updateDebouncer subMsg model

        DoNothing ->
            ( model, Cmd.none )


placedNodeAt : Position -> Int -> Node
placedNodeAt pos id =
    { id = id
    , pos = pos
    , dims = { width = 210.0, height = 90.0 }
    , text = ""
    , isSelected = False
    }


unplacedNodeAt : Position -> Node
unplacedNodeAt pos =
    placedNodeAt pos 0



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
    , SE.on "mousemove" (D.map (debouncedVersionOf TrackMouseMovementAt) mouseDecoder)
    ]


debouncedVersionOf : (a -> Msg) -> (a -> Msg)
debouncedVersionOf wrapInMsg a =
    --DebounceMsg (provideInput (wrapInMsg a))
    wrapInMsg a


svgAttributesWhileWaitingForNodeToBePlaced : Model -> List (S.Attribute Msg)
svgAttributesWhileWaitingForNodeToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            [ SE.on "click" (D.map PlaceNodeAt mouseDecoder)
            ]

        _ ->
            []


nodeElementToBePlaced : Model -> List (H.Html Msg)
nodeElementToBePlaced model =
    case model.state of
        WaitingForNodeToBePlaced ->
            case model.mouse.pos of
                Just pos ->
                    [ nodeElement
                        (unplacedNodeAt pos)
                        [ SA.stroke (colorToCssHsla unselectedNodeBorderColor)
                        , SA.fill "white"
                        , SA.opacity "0.6"
                        ]
                    ]

                Nothing ->
                    []

        _ ->
            []


placedNodeElements : Model -> List (S.Svg msg)
placedNodeElements model =
    List.map placedNodeElement model.nodes


placedNodeElement : Node -> S.Svg msg
placedNodeElement node =
    let
        nodeBorderColor =
            if node.isSelected then
                selectedNodeBorderColor

            else
                unselectedNodeBorderColor
    in
    nodeElement node
        [ SA.stroke (colorToCssHsla nodeBorderColor)
        , SA.fill (colorToCssHsla unselectedNodeFillColor)
        ]


nodeElement : Node -> List (S.Attribute msg) -> S.Svg msg
nodeElement node attrs =
    S.g
        [ SA.transform
            ("translate("
                ++ String.fromFloat node.pos.x
                ++ " "
                ++ String.fromFloat node.pos.y
                ++ ")"
            )
        ]
        (nodeBackground node attrs ++ nodeForeground node attrs)


nodeForeground : Node -> List (S.Attribute msg) -> List (S.Svg msg)
nodeForeground node attrs =
    let
        attrsWithPossibleStrokeWidth =
            if node.isSelected then
                attrs ++ [ SA.strokeWidth "2px" ]

            else
                attrs
    in
    [ roundedRect node.dims
        5
        (attrsWithPossibleStrokeWidth ++ [ SA.cursor "move" ])
    ]


nodeBackground : Node -> List (S.Attribute msg) -> List (S.Svg msg)
nodeBackground node attrs =
    if node.isSelected then
        let
            width =
                node.dims.width + 10

            height =
                node.dims.height + 10
        in
        [ S.rect
            (attrs
                ++ [ SA.x (String.fromFloat -(width / 2))
                   , SA.y (String.fromFloat -(height / 2))
                   , SA.width (String.fromFloat width)
                   , SA.height (String.fromFloat height)
                   , SA.fill (colorToCssHsla selectedNodeFillColor)
                   , SA.stroke "none"
                   ]
            )
            []
        ]

    else
        []


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


unselectedNodeBorderColor =
    hsla (degrees 228) 0.57 0.68 1



{-
   unplacedNodeBorderColor =
     unselectedNodeBorderColor
-}


selectedNodeBorderColor =
    hsla (degrees 228) 0.85 0.56 1


unselectedNodeFillColor =
    hsla (degrees 228) 0.56 0.91 1


selectedNodeFillColor =
    hsla (degrees 228) 0.56 0.91 0.6


joinIntsWith : String -> List Int -> String
joinIntsWith separator ints =
    String.join separator (List.map String.fromInt ints)
