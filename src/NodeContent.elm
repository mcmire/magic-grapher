port module NodeContent exposing
    ( Model
    , Msg
    , doNothing
    , endSelection
    , init
    , mapToDisplayDecodeError
    , startEditing
    , subscriptions
    , update
    , updateCursorPositionFromMouse
    , updateSelectionFromMouse
    , view
    )

import Browser.Events as BE
import CustomEventListeners exposing (handleCustomEvent)
import Html.Attributes as HA
import Json.Decode as D
import Json.Encode as E
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List
import Maybe.Extra
import NodeId exposing (NodeId)
import Regex
import String.Extra
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Types exposing (Position, Range)
import WordFinding
    exposing
        ( findNearestWordDownFrom
        , findNearestWordUpFrom
        , findSurroundingWord
        , findWordsIn
        )


port calculateGraphNodeContentMetrics : E.Value -> Cmd msg


port startListeningForNodeEditorKeyEvent : NodeId -> Cmd msg


port stopListeningForNodeEditorKeyEvent : () -> Cmd msg


port receiveNodeEditorKeyEvent : (E.Value -> msg) -> Sub msg


port updateGraphNodeEditorSelection : E.Value -> Cmd msg



-- MODEL


type alias SelectionLocation =
    { index : Int, position : Float }


type Anchor
    = Start
    | End


anchorToString : Anchor -> String
anchorToString anchor =
    case anchor of
        Start ->
            "Start"

        End ->
            "End"


decodeAnchor : String -> D.Decoder Anchor
decodeAnchor anchor =
    case anchor of
        "Start" ->
            D.succeed Start

        "End" ->
            D.succeed End

        _ ->
            D.fail "Anchor must be Start or End"


type UserLocation
    = Cursor SelectionLocation
    | Selection SelectionLocation SelectionLocation (Maybe Anchor)


type alias Model =
    { nodeId : NodeId
    , text : String
    , width : Float
    , height : Float
    , fontSize : Float
    , isBeingEdited : Bool
    , possibleUserLocation : Maybe UserLocation
    }


init : NodeId -> Model
init nodeId =
    { nodeId = nodeId
    , text = ""

    -- TODO: Nothing?
    , width = 0

    -- TODO: Nothing?
    , height = 0
    , fontSize = 16.0
    , isBeingEdited = False
    , possibleUserLocation = Nothing
    }



-- UPDATE


type Direction
    = Left
    | Right


type Unit
    = Char
    | Word


type SideOfLine
    = BeginningOfLine
    | EndOfLine


type Movement
    = JumpingBy Unit Direction
    | JumpingTo SideOfLine


type Change
    = InsertCharacter String
    | RemovePreviousCharacter
    | MoveCursor Movement
    | MoveSelection Movement


type QueuedSelectionUpdate
    = UpdateCursorPositionFromMouse Position
    | UpdateCursorPositionFromIndex Int
    | UpdateSelectionFromMouse Position
    | UpdateSelectionFromIndex Int Int Anchor


type alias MetricsRecalculatedEvent =
    { nodeId : NodeId
    , width : Float
    , height : Float
    , userLocation : UserLocation
    , text : String
    }


type Msg
    = DisplayDecodeError D.Error
    | DoNothing
    | EndSelection
    | QueueSelectionUpdate QueuedSelectionUpdate
    | ReceiveRecalculatedMetrics MetricsRecalculatedEvent
    | StartEditing
    | StopEditing
    | UpdateEditor Change


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EndSelection ->
            let
                newModel =
                    case model.possibleUserLocation of
                        Just (Selection start end possibleAnchor) ->
                            if start.index > end.index then
                                { model
                                    | possibleUserLocation =
                                        Just (Selection end start possibleAnchor)
                                }

                            else
                                model

                        _ ->
                            model
            in
            ( newModel, Cmd.none )

        StartEditing ->
            ( { model
                | isBeingEdited = True
                , possibleUserLocation = Just (Cursor { index = 0, position = 0 })
              }
            , startListeningForNodeEditorKeyEvent model.nodeId
            )

        StopEditing ->
            ( { model | isBeingEdited = False }
            , stopListeningForNodeEditorKeyEvent ()
            )

        QueueSelectionUpdate selectionUpdate ->
            ( model
            , updateGraphNodeEditorSelection
                (encodeUpdateGraphNodeEditorSelectionRequest
                    model
                    selectionUpdate
                )
            )

        UpdateEditor change ->
            case model.possibleUserLocation of
                Just userLocation ->
                    let
                        ( newModel, newSelectionUpdate ) =
                            case change of
                                InsertCharacter char ->
                                    case userLocation of
                                        Cursor loc ->
                                            let
                                                text =
                                                    String.Extra.insertAt char (loc.index + 1) model.text

                                                selectionUpdate =
                                                    UpdateCursorPositionFromIndex
                                                        (moveCursorRightByChar
                                                            loc.index
                                                            text
                                                        )
                                            in
                                            ( { model | text = text }, selectionUpdate )

                                        Selection start end _ ->
                                            let
                                                text =
                                                    String.slice 0 (start.index + 1) model.text
                                                        ++ char
                                                        ++ String.slice (end.index + 1) (String.length model.text) model.text

                                                selectionUpdate =
                                                    UpdateCursorPositionFromIndex (start.index + 1)
                                            in
                                            ( { model | text = text }, selectionUpdate )

                                RemovePreviousCharacter ->
                                    case userLocation of
                                        Cursor loc ->
                                            let
                                                text =
                                                    String.slice 0 loc.index model.text
                                                        ++ String.slice (loc.index + 1) (String.length model.text) model.text

                                                selectionUpdate =
                                                    UpdateCursorPositionFromIndex
                                                        (moveCursorLeftByChar loc.index text)
                                            in
                                            ( { model | text = text }, selectionUpdate )

                                        Selection start end _ ->
                                            let
                                                text =
                                                    String.slice 0 (start.index + 1) model.text
                                                        ++ String.slice (end.index + 1) (String.length model.text) model.text

                                                selectionUpdate =
                                                    UpdateCursorPositionFromIndex start.index
                                            in
                                            ( { model | text = text }, selectionUpdate )

                                MoveCursor movement ->
                                    let
                                        movementFn =
                                            case movement of
                                                JumpingBy unit Left ->
                                                    let
                                                        fn =
                                                            case unit of
                                                                Char ->
                                                                    moveCursorLeftByChar

                                                                Word ->
                                                                    moveCursorLeftByWord

                                                        index =
                                                            case userLocation of
                                                                Cursor loc ->
                                                                    loc.index

                                                                Selection start _ _ ->
                                                                    start.index
                                                    in
                                                    fn index

                                                JumpingBy unit Right ->
                                                    let
                                                        fn =
                                                            case unit of
                                                                Char ->
                                                                    moveCursorRightByChar

                                                                Word ->
                                                                    moveCursorRightByWord

                                                        index =
                                                            case userLocation of
                                                                Cursor loc ->
                                                                    loc.index

                                                                Selection _ end _ ->
                                                                    end.index
                                                    in
                                                    fn index

                                                JumpingTo BeginningOfLine ->
                                                    moveCursorToBeginningOfLine

                                                JumpingTo EndOfLine ->
                                                    moveCursorToEndOfLine

                                        selectionUpdate =
                                            UpdateCursorPositionFromIndex (movementFn model.text)
                                    in
                                    ( model, selectionUpdate )

                                MoveSelection movement ->
                                    let
                                        selectionUpdate =
                                            case movement of
                                                JumpingBy unit Left ->
                                                    let
                                                        fn =
                                                            case unit of
                                                                Char ->
                                                                    moveCursorLeftByChar

                                                                Word ->
                                                                    moveCursorLeftByWord

                                                        ( startIndex, endIndex, anchor ) =
                                                            case userLocation of
                                                                Cursor loc ->
                                                                    ( fn loc.index model.text, loc.index, Start )

                                                                Selection start end (Just End) ->
                                                                    ( start.index, fn end.index model.text, End )

                                                                Selection start end _ ->
                                                                    ( fn start.index model.text, end.index, Start )
                                                    in
                                                    UpdateSelectionFromIndex startIndex endIndex anchor

                                                JumpingBy unit Right ->
                                                    let
                                                        fn =
                                                            case unit of
                                                                Char ->
                                                                    moveCursorRightByChar

                                                                Word ->
                                                                    moveCursorRightByWord

                                                        ( startIndex, endIndex, anchor ) =
                                                            case userLocation of
                                                                Cursor loc ->
                                                                    ( loc.index, fn loc.index model.text, End )

                                                                Selection start end (Just Start) ->
                                                                    ( fn start.index model.text, end.index, Start )

                                                                Selection start end _ ->
                                                                    ( start.index, fn end.index model.text, End )
                                                    in
                                                    UpdateSelectionFromIndex startIndex endIndex anchor

                                                JumpingTo BeginningOfLine ->
                                                    case userLocation of
                                                        Cursor loc ->
                                                            UpdateSelectionFromIndex
                                                                (moveCursorToBeginningOfLine model.text)
                                                                loc.index
                                                                Start

                                                        Selection _ end anchor ->
                                                            UpdateSelectionFromIndex
                                                                (moveCursorToBeginningOfLine model.text)
                                                                end.index
                                                                (Maybe.withDefault Start anchor)

                                                JumpingTo EndOfLine ->
                                                    case userLocation of
                                                        Cursor loc ->
                                                            UpdateSelectionFromIndex
                                                                loc.index
                                                                (moveCursorToEndOfLine model.text)
                                                                End

                                                        Selection start _ anchor ->
                                                            UpdateSelectionFromIndex
                                                                start.index
                                                                (moveCursorToEndOfLine model.text)
                                                                (Maybe.withDefault End anchor)
                                    in
                                    ( model, selectionUpdate )

                        cmd =
                            updateGraphNodeEditorSelection
                                (encodeUpdateGraphNodeEditorSelectionRequest
                                    newModel
                                    newSelectionUpdate
                                )
                    in
                    ( model, cmd )

                Nothing ->
                    ( model, Cmd.none )

        ReceiveRecalculatedMetrics event ->
            ( { model
                | width = event.width
                , height = event.height
                , possibleUserLocation = Just event.userLocation
                , text = event.text
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


encodeUpdateGraphNodeEditorSelectionRequest : Model -> QueuedSelectionUpdate -> E.Value
encodeUpdateGraphNodeEditorSelectionRequest model selectionUpdate =
    let
        attrsWithPossibleMousePosition attrs =
            case selectionUpdate of
                UpdateCursorPositionFromMouse pos ->
                    attrs
                        ++ [ ( "type"
                             , E.string "UpdateCursorPositionFromMouse"
                             )
                           , ( "at"
                             , E.object
                                [ ( "x", E.float pos.x )
                                , ( "y", E.float pos.y )
                                ]
                             )
                           ]

                UpdateCursorPositionFromIndex index ->
                    attrs
                        ++ [ ( "type"
                             , E.string "UpdateCursorPositionFromIndex"
                             )
                           , ( "at"
                             , E.int index
                             )
                           ]

                UpdateSelectionFromMouse newPos ->
                    case model.possibleUserLocation of
                        Just userLocation ->
                            attrs
                                ++ [ ( "type", E.string "UpdateSelectionFromMouse" )
                                   , ( "from"
                                     , E.int (flattenUserLocation userLocation).index
                                     )
                                   , ( "to"
                                     , E.object
                                        [ ( "x", E.float newPos.x )
                                        , ( "y", E.float newPos.y )
                                        ]
                                     )
                                   ]

                        Nothing ->
                            attrs

                UpdateSelectionFromIndex startIndex endIndex anchor ->
                    attrs
                        ++ [ ( "type", E.string "UpdateSelectionFromIndex" )
                           , ( "from", E.int startIndex )
                           , ( "to", E.int endIndex )
                           , ( "anchor", E.string (anchorToString anchor) )
                           ]
    in
    E.object
        (attrsWithPossibleMousePosition
            [ ( "graphNodeId", NodeId.encode model.nodeId )
            , ( "text", E.string model.text )
            ]
        )


flattenUserLocation : UserLocation -> SelectionLocation
flattenUserLocation userLocation =
    case userLocation of
        Selection start _ _ ->
            start

        Cursor loc ->
            loc


moveCursorLeftByChar : Int -> String -> Int
moveCursorLeftByChar cursorIndex text =
    normalizeCursorIndex (cursorIndex - 1) text


moveCursorLeftByWord : Int -> String -> Int
moveCursorLeftByWord cursorIndex text =
    let
        words =
            findWordsIn text

        possibleSurroundingWord =
            -- TODO: Maybe a way to simplify this?
            case findSurroundingWord cursorIndex words of
                Just word ->
                    if cursorIndex > word.startIndex then
                        Just word

                    else
                        Nothing

                Nothing ->
                    Nothing

        possibleWord =
            Maybe.Extra.or
                possibleSurroundingWord
                (findNearestWordDownFrom (cursorIndex - 1) words)

        newCursorIndex =
            case possibleWord of
                Just word ->
                    word.startIndex - 1

                Nothing ->
                    -1
    in
    normalizeCursorIndex newCursorIndex text


moveCursorRightByChar : Int -> String -> Int
moveCursorRightByChar cursorIndex text =
    normalizeCursorIndex (cursorIndex + 1) text


moveCursorRightByWord : Int -> String -> Int
moveCursorRightByWord cursorIndex text =
    let
        words =
            findWordsIn text

        _ =
            Debug.log "words" words

        possibleSurroundingWord =
            case findSurroundingWord cursorIndex words of
                Just word ->
                    if cursorIndex < word.endIndex then
                        Just word

                    else
                        Nothing

                Nothing ->
                    Nothing

        possibleWord =
            Maybe.Extra.or
                possibleSurroundingWord
                (findNearestWordUpFrom (cursorIndex + 1) words)

        newCursorIndex =
            case possibleWord of
                Just word ->
                    word.endIndex

                Nothing ->
                    String.length text - 1
    in
    normalizeCursorIndex newCursorIndex text


moveCursorToBeginningOfLine : String -> Int
moveCursorToBeginningOfLine text =
    normalizeCursorIndex -1 text


moveCursorToEndOfLine : String -> Int
moveCursorToEndOfLine text =
    normalizeCursorIndex (String.length text - 1) text


normalizeCursorIndex : Int -> String -> Int
normalizeCursorIndex index text =
    if index < -1 then
        -1

    else if index > String.length text - 1 then
        String.length text - 1

    else
        index


mapToDisplayDecodeError : Msg -> Maybe D.Error
mapToDisplayDecodeError msg =
    case msg of
        DisplayDecodeError error ->
            Just error

        _ ->
            Nothing


doNothing : Msg
doNothing =
    DoNothing


startEditing : Msg
startEditing =
    -- TODO: Any way to avoid this??
    StartEditing


updateCursorPositionFromMouse : Position -> Msg
updateCursorPositionFromMouse pos =
    QueueSelectionUpdate (UpdateCursorPositionFromMouse pos)


updateSelectionFromMouse : Position -> Msg
updateSelectionFromMouse pos =
    QueueSelectionUpdate (UpdateSelectionFromMouse pos)


endSelection : Msg
endSelection =
    EndSelection



-- VIEW


view : Model -> List (S.Attribute Msg) -> S.Svg Msg
view model attrs =
    let
        textViews =
            [ textView model attrs ]

        cursorViews =
            cursorView model attrs

        selectionViews =
            selectionView model attrs
    in
    S.g [] (selectionViews ++ textViews ++ cursorViews)


textView : Model -> List (S.Attribute Msg) -> S.Svg Msg
textView model attrs =
    -- TODO: Remove attrs
    S.text_
        ([]
            ++ [ SA.x (String.fromFloat 0)
               , SA.y (String.fromFloat 0)
               , SA.dominantBaseline "central"
               , SA.textAnchor "middle"
               , SA.fontSize (String.fromFloat model.fontSize ++ "px")
               , HA.attribute "data-id" "graph-node-editor"
               , HA.attribute "data-graph-node-id" (String.fromInt model.nodeId)
               , HA.attribute "data-width"
                    (String.fromFloat model.width)
               , HA.attribute "data-height"
                    (String.fromFloat model.height)
               , SE.on "init" (D.succeed StartEditing)
               , SE.on "key"
                    (handleCustomEvent
                        { decodeValue = decodeKeyEvent
                        , onSuccess = mapKeyboardEventToMsg model
                        , onError = DisplayDecodeError
                        }
                    )
               , SE.on "metricsRecalculated"
                    (handleCustomEvent
                        { decodeValue = decodeMetricsRecalculatedEvent
                        , onSuccess = ReceiveRecalculatedMetrics
                        , onError = DisplayDecodeError
                        }
                    )
               ]
        )
        [ S.text (normalizeTextForSvgElement model.text) ]


decodeMetricsRecalculatedEvent : D.Decoder MetricsRecalculatedEvent
decodeMetricsRecalculatedEvent =
    D.field "detail"
        (D.map5
            MetricsRecalculatedEvent
            (D.field "graphNodeId" NodeId.decode)
            (D.field "width" D.float)
            (D.field "height" D.float)
            (D.field "userLocation"
                (D.oneOf
                    [ D.map
                        Cursor
                        (D.map2 SelectionLocation
                            (D.field "index" D.int)
                            (D.field "position" D.float)
                        )
                    , D.map3 Selection
                        (D.field "start"
                            (D.map2 SelectionLocation
                                (D.field "index" D.int)
                                (D.field "position" D.float)
                            )
                        )
                        (D.field "end"
                            (D.map2 SelectionLocation
                                (D.field "index" D.int)
                                (D.field "position" D.float)
                            )
                        )
                        (D.field "anchor" (D.nullable (D.string |> D.andThen decodeAnchor)))
                    ]
                )
            )
            (D.field "text" D.string)
        )


decodeKeyEvent : D.Decoder KeyboardEvent
decodeKeyEvent =
    D.field "detail" decodeKeyboardEvent


normalizeTextForSvgElement : String -> String
normalizeTextForSvgElement text =
    let
        leadingSpaceRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[ ]"

        trailingSpaceRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[ ]$"
    in
    text
        |> Regex.replace leadingSpaceRegex (\_ -> "\u{00A0}")
        |> Regex.replace trailingSpaceRegex (\_ -> "\u{00A0}")


cursorView : Model -> List (S.Attribute msg) -> List (S.Svg msg)
cursorView model attrs =
    if model.isBeingEdited then
        case model.possibleUserLocation of
            Just (Cursor loc) ->
                let
                    -- TODO: Use
                    nodePadding =
                        10.0
                in
                [ S.rect
                    [ SA.x (String.fromFloat loc.position)
                    , SA.y (String.fromFloat (-model.fontSize / 2))
                    , SA.width "2"
                    , SA.height (String.fromFloat model.fontSize)
                    , SA.shapeRendering "crispEdges"
                    , HA.attribute "data-testid" "cursor"
                    , SA.class "cursor hidden"
                    , HA.attribute "data-id" "graph-node-editor-cursor"
                    , HA.attribute "data-graph-node-id" (String.fromInt model.nodeId)
                    ]
                    []
                ]

            _ ->
                []

    else
        []


selectionView : Model -> List (S.Attribute msg) -> List (S.Svg msg)
selectionView model attrs =
    if model.isBeingEdited then
        case model.possibleUserLocation of
            Just (Selection start end _) ->
                let
                    ( normalizedStart, normalizedEnd ) =
                        if end.position > start.position then
                            ( start, end )

                        else
                            ( end, start )
                in
                [ S.rect
                    [ SA.x (String.fromFloat normalizedStart.position)
                    , SA.y (String.fromFloat (-model.fontSize / 2))
                    , SA.width (String.fromFloat (normalizedEnd.position - normalizedStart.position))
                    , SA.height (String.fromFloat model.fontSize)
                    , SA.class "selection"
                    , HA.attribute "data-testid" "selection"
                    ]
                    []
                ]

            _ ->
                []

    else
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


mapKeyboardEventToMsg : Model -> KeyboardEvent -> Msg
mapKeyboardEventToMsg model keyboardEvent =
    if model.isBeingEdited then
        case keyboardEvent.keyCode of
            Key.Escape ->
                -- TODO: Make sure cursor goes away
                StopEditing

            Key.Backspace ->
                -- TODO: Implement removing previous word, line
                UpdateEditor RemovePreviousCharacter

            Key.Left ->
                if onlyShiftKeyPressed keyboardEvent then
                    -- FIXXXXXXX
                    UpdateEditor (MoveSelection (JumpingBy Char Left))

                else if onlyAltKeyPressed keyboardEvent then
                    UpdateEditor (MoveCursor (JumpingBy Word Left))

                else if onlyMetaKeyPressed keyboardEvent then
                    UpdateEditor (MoveCursor (JumpingTo BeginningOfLine))

                else if shiftAltKeyPressed keyboardEvent then
                    UpdateEditor (MoveSelection (JumpingBy Word Left))

                else if shiftMetaKeyPressed keyboardEvent then
                    UpdateEditor (MoveSelection (JumpingTo BeginningOfLine))

                else if noModifierKeysPressed keyboardEvent then
                    UpdateEditor (MoveCursor (JumpingBy Char Left))

                else
                    DoNothing

            Key.Right ->
                if onlyShiftKeyPressed keyboardEvent then
                    UpdateEditor (MoveSelection (JumpingBy Char Right))

                else if onlyAltKeyPressed keyboardEvent then
                    UpdateEditor (MoveCursor (JumpingBy Word Right))

                else if onlyMetaKeyPressed keyboardEvent then
                    UpdateEditor (MoveCursor (JumpingTo EndOfLine))

                else if shiftAltKeyPressed keyboardEvent then
                    UpdateEditor (MoveSelection (JumpingBy Word Right))

                else if shiftMetaKeyPressed keyboardEvent then
                    UpdateEditor (MoveSelection (JumpingTo EndOfLine))

                else if noModifierKeysPressed keyboardEvent then
                    UpdateEditor (MoveCursor (JumpingBy Char Right))

                else
                    DoNothing

            _ ->
                case keyboardEvent.key of
                    Just key ->
                        if String.length key == 1 then
                            UpdateEditor (InsertCharacter key)

                        else
                            DoNothing

                    Nothing ->
                        DoNothing

    else
        DoNothing


onlyAltKeyPressed : KeyboardEvent -> Bool
onlyAltKeyPressed keyboardEvent =
    keyboardEvent.altKey
        && not keyboardEvent.ctrlKey
        && not keyboardEvent.shiftKey
        && not keyboardEvent.metaKey


shiftAltKeyPressed : KeyboardEvent -> Bool
shiftAltKeyPressed keyboardEvent =
    keyboardEvent.shiftKey
        && keyboardEvent.altKey
        && not keyboardEvent.ctrlKey
        && not keyboardEvent.metaKey


onlyMetaKeyPressed : KeyboardEvent -> Bool
onlyMetaKeyPressed keyboardEvent =
    keyboardEvent.metaKey
        && not keyboardEvent.ctrlKey
        && not keyboardEvent.altKey
        && not keyboardEvent.shiftKey


shiftMetaKeyPressed : KeyboardEvent -> Bool
shiftMetaKeyPressed keyboardEvent =
    keyboardEvent.shiftKey
        && keyboardEvent.metaKey
        && not keyboardEvent.ctrlKey
        && not keyboardEvent.altKey


onlyShiftKeyPressed : KeyboardEvent -> Bool
onlyShiftKeyPressed keyboardEvent =
    keyboardEvent.shiftKey
        && not keyboardEvent.ctrlKey
        && not keyboardEvent.altKey
        && not keyboardEvent.metaKey


noModifierKeysPressed : KeyboardEvent -> Bool
noModifierKeysPressed keyboardEvent =
    not keyboardEvent.altKey
        && not keyboardEvent.ctrlKey
        && not keyboardEvent.metaKey
        && not keyboardEvent.shiftKey
