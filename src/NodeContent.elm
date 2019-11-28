port module NodeContent exposing
    ( Model
    , Msg
    ,  doNothing
       --, endSelection

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
import List.Extra
import Maybe.Extra
import NodeId exposing (NodeId)
import Regex
import String.Extra
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Types exposing (Position, Range)


port calculateGraphNodeContentMetrics : E.Value -> Cmd msg


port startListeningForNodeEditorKeyEvent : NodeId -> Cmd msg


port stopListeningForNodeEditorKeyEvent : () -> Cmd msg


port receiveNodeEditorKeyEvent : (E.Value -> msg) -> Sub msg


port updateGraphNodeEditorSelection : E.Value -> Cmd msg



-- MODEL


type alias SelectionLocation =
    { index : Int, position : Float }


type alias SelectionRange =
    { start : SelectionLocation, end : SelectionLocation }


type UserLocation
    = Cursor SelectionLocation
    | Selection SelectionRange


type alias Model =
    { nodeId : NodeId
    , text : String
    , width : Float
    , height : Float
    , fontSize : Float
    , isBeingEdited : Bool
    , userLocation : Maybe UserLocation
    }


type alias Word =
    { startIndex : Int, endIndex : Int }


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
    , userLocation = Nothing
    }



-- UPDATE


type Change
    = InsertCharacter String
    | RemovePreviousCharacter
    | MoveCursorLeftByChar
    | MoveCursorLeftByWord
    | MoveCursorRightByChar
    | MoveCursorRightByWord
    | MoveCursorToBeginningOfLine
    | MoveCursorToEndOfLine


type QueuedSelectionUpdate
    = UpdateCursorPositionFromMouse Position
    | UpdateCursorPositionFromIndex Int
    | UpdateSelectionFromMouse Position



--| EndSelection


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
    | ReceiveRecalculatedMetrics MetricsRecalculatedEvent
    | StartEditing
    | StopEditing
    | UpdateEditor Change
    | QueueSelectionUpdate QueuedSelectionUpdate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartEditing ->
            ( { model
                | isBeingEdited = True
                , userLocation = Just (Cursor { index = 0, position = 0 })
              }
            , startListeningForNodeEditorKeyEvent model.nodeId
            )

        StopEditing ->
            ( { model | isBeingEdited = False }
            , stopListeningForNodeEditorKeyEvent ()
            )

        {-
           MouseDown pos ->
               ( { model | isMouseDown = True },
               updateGraphNodeEditorSelection
                   (encodeUpdateGraphNodeEditorSelectionRequest
                       model.nodeId
                       (UpdateCursorPositionFromMouse pos)
                   )
                )

           MouseUp ->
        -}
        QueueSelectionUpdate selectionUpdate ->
            ( model
            , updateGraphNodeEditorSelection
                (encodeUpdateGraphNodeEditorSelectionRequest
                    model
                    selectionUpdate
                )
            )

        -- This is just responsible for key events right now -- no mouse events
        UpdateEditor change ->
            case model.userLocation of
                Just (Cursor cursor) ->
                    let
                        text =
                            case change of
                                InsertCharacter char ->
                                    String.Extra.insertAt char cursor.index model.text

                                RemovePreviousCharacter ->
                                    String.slice 0 (cursor.index - 1) model.text
                                        ++ String.slice cursor.index (String.length model.text) model.text

                                _ ->
                                    model.text

                        newCursorIndex =
                            case change of
                                InsertCharacter char ->
                                    moveCursorRightByChar cursor.index text

                                RemovePreviousCharacter ->
                                    moveCursorLeftByChar cursor.index text

                                MoveCursorLeftByChar ->
                                    moveCursorLeftByChar cursor.index text

                                MoveCursorLeftByWord ->
                                    moveCursorLeftByWord cursor.index text

                                MoveCursorRightByChar ->
                                    moveCursorRightByChar cursor.index text

                                MoveCursorRightByWord ->
                                    moveCursorRightByWord cursor.index text

                                MoveCursorToBeginningOfLine ->
                                    moveCursorToBeginningOfLine text

                                MoveCursorToEndOfLine ->
                                    moveCursorToEndOfLine text

                        selectionUpdate =
                            UpdateCursorPositionFromIndex newCursorIndex

                        cmd =
                            updateGraphNodeEditorSelection
                                (encodeUpdateGraphNodeEditorSelectionRequest
                                    { model | text = text }
                                    selectionUpdate
                                )
                    in
                    ( model, cmd )

                Just (Selection _) ->
                    ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ReceiveRecalculatedMetrics event ->
            ( { model
                | width = event.width
                , height = event.height
                , userLocation = Just event.userLocation
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
                    case model.userLocation of
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

        --EndSelection ->
        --attrs ++ [ ( "type", E.string "EndSelection" ) ]
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
        Selection range ->
            range.start

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
                (findNearestWordDownFrom cursorIndex words)

        newCursorIndex =
            case possibleWord of
                Just word ->
                    word.startIndex

                Nothing ->
                    0
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
                (findNearestWordUpFrom cursorIndex words)

        newCursorIndex =
            case possibleWord of
                Just word ->
                    word.endIndex

                Nothing ->
                    String.length text
    in
    normalizeCursorIndex newCursorIndex text


moveCursorToBeginningOfLine : String -> Int
moveCursorToBeginningOfLine text =
    normalizeCursorIndex 0 text


moveCursorToEndOfLine : String -> Int
moveCursorToEndOfLine text =
    normalizeCursorIndex (String.length text) text


findSurroundingWord : Int -> List Word -> Maybe Word
findSurroundingWord index words =
    List.Extra.find
        (\word -> index >= word.startIndex && index <= word.endIndex)
        words


findNearestWordDownFrom : Int -> List Word -> Maybe Word
findNearestWordDownFrom index words =
    List.Extra.find
        (\word -> index >= word.endIndex)
        (List.reverse words)


findNearestWordUpFrom : Int -> List Word -> Maybe Word
findNearestWordUpFrom index words =
    List.Extra.find
        (\word -> index <= word.startIndex)
        words


normalizeCursorIndex : Int -> String -> Int
normalizeCursorIndex index text =
    if index < -1 then
        -1

    else if index > String.length text - 1 then
        String.length text - 1

    else
        index


findWordsIn : String -> List Word
findWordsIn text =
    let
        regex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[\\w_]+"

        matches =
            Regex.find regex text
    in
    List.map
        (\match ->
            { startIndex = match.index
            , endIndex = match.index + String.length match.match
            }
        )
        matches



{-
   encodeCalculateGraphNodeContentMetricsRequest : NodeId -> Int -> E.Value
   encodeCalculateGraphNodeContentMetricsRequest nodeId cursorIndex =
       E.object
           [ ( "graphNodeId", NodeId.encode nodeId )
           , ( "cursorSelectionAsIndices", E.int cursorIndex )
           , ( "text", E.string model.text )
           ]
-}


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



{-
   endSelection : Msg
   endSelection =
       QueueSelectionUpdate EndSelection
-}
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
                    , D.map Selection
                        (D.map2 SelectionRange
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
                        )
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
        case model.userLocation of
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
                    , SA.class "blink"
                    , HA.attribute "data-testid" "cursor"
                    , SA.class "cursor"
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
        case model.userLocation of
            Just (Selection range) ->
                let
                    normalizedRange =
                        if range.end.position > range.start.position then
                            range

                        else
                            { start = range.end, end = range.start }
                in
                [ S.rect
                    [ SA.x (String.fromFloat normalizedRange.start.position)
                    , SA.y (String.fromFloat (-model.fontSize / 2))
                    , SA.width (String.fromFloat (normalizedRange.end.position - normalizedRange.start.position))
                    , SA.height (String.fromFloat model.fontSize)
                    , SA.class "selection"
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
                StopEditing

            Key.Backspace ->
                -- TODO: Implement removing previous word, line
                UpdateEditor RemovePreviousCharacter

            Key.Left ->
                if onlyAltKeyPressed keyboardEvent then
                    UpdateEditor MoveCursorLeftByWord

                else if onlyMetaKeyPressed keyboardEvent then
                    UpdateEditor MoveCursorToBeginningOfLine

                else if noModifierKeysPressed keyboardEvent then
                    UpdateEditor MoveCursorLeftByChar

                else
                    DoNothing

            Key.Right ->
                if onlyAltKeyPressed keyboardEvent then
                    UpdateEditor MoveCursorRightByWord

                else if onlyMetaKeyPressed keyboardEvent then
                    UpdateEditor MoveCursorToEndOfLine

                else if noModifierKeysPressed keyboardEvent then
                    UpdateEditor MoveCursorRightByChar

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
        && not keyboardEvent.metaKey
        && not keyboardEvent.shiftKey


onlyMetaKeyPressed : KeyboardEvent -> Bool
onlyMetaKeyPressed keyboardEvent =
    not keyboardEvent.altKey
        && not keyboardEvent.ctrlKey
        && keyboardEvent.metaKey
        && not keyboardEvent.shiftKey


noModifierKeysPressed : KeyboardEvent -> Bool
noModifierKeysPressed keyboardEvent =
    not keyboardEvent.altKey
        && not keyboardEvent.ctrlKey
        && not keyboardEvent.metaKey
        && not keyboardEvent.shiftKey
