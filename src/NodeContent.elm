module NodeContent exposing
    ( NodeContent
    , init
    , moveCursorLeftByChar
    , moveCursorLeftByWord
    , moveCursorRightByChar
    , moveCursorRightByWord
    , normalizeTextForSvgElement
    )

import List
import List.Extra
import Maybe.Extra
import Regex
import Types exposing (Range)


type alias NodeContent =
    { text : String
    , width : Float
    , height : Float
    , fontSize : Float
    , isBeingEdited : Bool
    , cursorIndex : Int
    , cursorPosition : Float
    }


type alias Word =
    { startIndex : Int, endIndex : Int }


init : NodeContent
init =
    { text = ""

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
