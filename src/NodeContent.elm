port module NodeContent exposing
    ( Model
    , Msg
    , doNothing
    , init
    , mapToDisplayDecodeError
    , startEditing
    , subscriptions
    , update
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
import Styles.NodeContent as Styles
import Svg as S
import Svg.Attributes as SA
import Svg.Events as SE
import Types exposing (Range)


port calculateNodeContentMetrics : E.Value -> Cmd msg


port startListeningForNodeEditorKeyEvent : NodeId -> Cmd msg


port stopListeningForNodeEditorKeyEvent : () -> Cmd msg


port receiveNodeEditorKeyEvent : (E.Value -> msg) -> Sub msg



-- MODEL


type alias Model =
    { nodeId : NodeId
    , text : String
    , width : Float
    , height : Float
    , fontSize : Float
    , isBeingEdited : Bool
    , cursorIndex : Int
    , cursorPosition : Float
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

    -- TODO: Nothing?
    , cursorIndex = 0

    -- TODO: Nothing?
    , cursorPosition = 0.0
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


type alias MetricsRecalculatedEvent =
    { nodeId : NodeId
    , width : Float
    , height : Float
    , cursorPosition : Float
    , text : String
    }


type Msg
    = DisplayDecodeError D.Error
    | DoNothing
    | ReceiveRecalculatedMetrics MetricsRecalculatedEvent
    | StartEditing
    | StopEditing
    | UpdateEditor Change


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartEditing ->
            ( { model | isBeingEdited = True }
            , startListeningForNodeEditorKeyEvent model.nodeId
            )

        StopEditing ->
            ( { model | isBeingEdited = False }
            , stopListeningForNodeEditorKeyEvent ()
            )

        UpdateEditor change ->
            let
                text =
                    case change of
                        InsertCharacter char ->
                            String.Extra.insertAt
                                char
                                model.cursorIndex
                                model.text

                        RemovePreviousCharacter ->
                            String.slice 0 (model.cursorIndex - 1) model.text
                                ++ String.slice
                                    model.cursorIndex
                                    (String.length model.text)
                                    model.text

                        _ ->
                            model.text

                cursorIndexFn =
                    case change of
                        InsertCharacter char ->
                            moveCursorRightByChar model.cursorIndex

                        RemovePreviousCharacter ->
                            moveCursorLeftByChar model.cursorIndex

                        MoveCursorLeftByChar ->
                            moveCursorLeftByChar model.cursorIndex

                        MoveCursorLeftByWord ->
                            moveCursorLeftByWord model.cursorIndex

                        MoveCursorRightByChar ->
                            moveCursorRightByChar model.cursorIndex

                        MoveCursorRightByWord ->
                            moveCursorRightByWord model.cursorIndex

                        MoveCursorToBeginningOfLine ->
                            moveCursorToBeginningOfLine

                        MoveCursorToEndOfLine ->
                            moveCursorToEndOfLine

                cursorIndex =
                    cursorIndexFn text

                newModel =
                    { model | text = text, cursorIndex = cursorIndex }

                cmd =
                    calculateNodeContentMetrics
                        (encodeCalculateMetricsRequest newModel)
            in
            ( newModel, cmd )

        ReceiveRecalculatedMetrics event ->
            ( { model
                | width = event.width
                , height = event.height
                , cursorPosition = event.cursorPosition
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


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
    if index < 0 then
        0

    else if index > String.length text then
        String.length text

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


encodeCalculateMetricsRequest : Model -> E.Value
encodeCalculateMetricsRequest model =
    E.object
        [ ( "nodeId", NodeId.encode model.nodeId )
        , ( "cursorIndex", E.int model.cursorIndex )
        , ( "text", E.string model.text )
        ]


doNothing : Msg
doNothing =
    DoNothing


startEditing : Msg
startEditing =
    -- TODO: Any way to avoid this??
    StartEditing


mapToDisplayDecodeError : Msg -> Maybe D.Error
mapToDisplayDecodeError msg =
    case msg of
        DisplayDecodeError error ->
            Just error

        _ ->
            Nothing



-- VIEW


view : Model -> List (S.Attribute Msg) -> S.Svg Msg
view model attrs =
    let
        textViews =
            [ textView model attrs ]

        cursorViews =
            cursorView model attrs
    in
    S.g [] (textViews ++ cursorViews)


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
               , HA.attribute "data-id" "editor-text"
               , HA.attribute "data-node-id" (String.fromInt model.nodeId)
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
            (D.field "nodeId" NodeId.decode)
            (D.field "width" D.float)
            (D.field "height" D.float)
            (D.field "cursorPosition" D.float)
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
        let
            -- TODO: Use
            nodePadding =
                10.0
        in
        [ S.rect
            [ SA.x (String.fromFloat model.cursorPosition)
            , SA.y (String.fromFloat (-model.fontSize / 2))
            , SA.width "2"
            , SA.height (String.fromFloat model.fontSize)
            , SA.shapeRendering "crispEdges"
            , SA.class Styles.blink
            , HA.attribute "data-testid" "cursor"
            ]
            []
        ]

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
