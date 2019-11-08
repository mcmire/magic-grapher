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

import Browser.Events exposing (onKeyDown)
import CustomEventListeners exposing (handleCustomEvent)
import Debug
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



-- UPDATE


type Change
    = InsertCharacter String
    | RemovePreviousCharacter
    | MoveCursorLeftByChar
    | MoveCursorLeftByWord
    | MoveCursorRightByChar
    | MoveCursorRightByWord


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
            ( { model | isBeingEdited = True }, Cmd.none )

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
                            let
                                _ =
                                    Debug.log "Removing character at index" { index = model.cursorIndex - 1 }
                            in
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
                            moveCursorRightByChar

                        RemovePreviousCharacter ->
                            moveCursorLeftByChar

                        MoveCursorLeftByChar ->
                            moveCursorLeftByChar

                        MoveCursorLeftByWord ->
                            moveCursorLeftByWord

                        MoveCursorRightByChar ->
                            moveCursorRightByChar

                        MoveCursorRightByWord ->
                            moveCursorRightByWord

                cursorIndex =
                    cursorIndexFn model.cursorIndex text

                newModel =
                    { model | text = text, cursorIndex = cursorIndex }

                cmd =
                    calculateNodeContentMetrics
                        (encodeCalculateMetricsRequest newModel)
            in
            ( newModel, cmd )

        ReceiveRecalculatedMetrics event ->
            let
                _ =
                    Debug.log "[Elm] receiving recalculated node content metrics" event
            in
            ( { model
                | width = event.width
                , height = event.height
                , cursorPosition = event.cursorPosition
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


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
    S.g
        [ HA.attribute "data-id" "node-editor"
        , HA.attribute "data-node-id" (String.fromInt model.nodeId)
        ]
        (textViews ++ cursorViews)


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
               , HA.attribute "data-id" "text"
               , HA.attribute "data-width"
                    (String.fromFloat model.width)
               , HA.attribute "data-height"
                    (String.fromFloat model.height)
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


decodeCharacterPosition : D.Decoder Range
decodeCharacterPosition =
    D.map2
        Range
        (D.field "start" D.int)
        (D.field "end" D.int)


normalizeTextForSvgElement : String -> String
normalizeTextForSvgElement text =
    case String.uncons (String.reverse text) of
        Just ( lastChar, rest ) ->
            if lastChar == ' ' then
                String.reverse (String.cons '\u{00A0}' rest)

            else
                text

        Nothing ->
            text


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
            ]
            []
        ]

    else
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isBeingEdited then
        onKeyDown (D.map (mapKeyboardEventToMsg model) decodeKeyboardEvent)

    else
        Sub.none


mapKeyboardEventToMsg : Model -> KeyboardEvent -> Msg
mapKeyboardEventToMsg model keyboardEvent =
    if model.isBeingEdited then
        case keyboardEvent.keyCode of
            Key.Escape ->
                StopEditing

            Key.Backspace ->
                UpdateEditor RemovePreviousCharacter

            Key.Left ->
                if keyboardEvent.altKey then
                    UpdateEditor MoveCursorLeftByWord

                else
                    UpdateEditor MoveCursorLeftByChar

            Key.Right ->
                if keyboardEvent.altKey then
                    UpdateEditor MoveCursorRightByWord

                else
                    UpdateEditor MoveCursorRightByChar

            _ ->
                let
                    _ =
                        Debug.log "Key" keyboardEvent.key
                in
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
