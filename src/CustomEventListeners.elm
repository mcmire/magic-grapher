module CustomEventListeners exposing (handleCustomEvent)

import Json.Decode as D


handleCustomEvent :
    { decodeValue : D.Decoder value
    , onSuccess : value -> msg
    , onError : D.Error -> msg
    }
    -> D.Decoder msg
handleCustomEvent { decodeValue, onSuccess, onError } =
    -- This looks particularly heinous, but Elm "conveniently" fails silently if
    -- it cannot decode the data within the event which is emitted on the
    -- JavaScript side.
    --
    -- Source for this solution: <https://github.com/Janiczek/elm-docs/issues/40>
    --
    D.value
        |> D.andThen
            (\value ->
                case D.decodeValue decodeValue value of
                    Ok event ->
                        D.succeed (onSuccess event)

                    Err err ->
                        D.succeed (onError err)
            )
