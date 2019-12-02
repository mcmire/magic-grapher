module WordFinding exposing
    ( findNearestWordDownFrom
    , findNearestWordUpFrom
    , findSurroundingWord
    , findWordsIn
    )

import List.Extra
import Regex


type alias Word =
    { startIndex : Int, endIndex : Int, text : String }


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
            let
                startIndex =
                    match.index

                endIndex =
                    match.index + String.length match.match - 1
            in
            { startIndex = startIndex
            , endIndex = endIndex
            , text = String.slice startIndex (endIndex + 1) text
            }
        )
        matches
