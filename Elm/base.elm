module Base exposing (..)

import Dict exposing (..)
import Date

import Json.Decode as D
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional, hardcoded)


type alias Competition =
    { id : String
    , name : String
    , events : List String
    , date : Date.Date
    , competitors : List Competitor
    }

type alias Competitor =
    { id : String
    , events : List String
    }

type alias Person =
    { id : String
    , name : String
    , times : Dict String (List Time) -- Event: [Time]
    }

type Time 
    = Time Float
    | DNF

stringify : Time -> String
stringify x = 
    case x of
        Time y -> toString y
        DNF -> "DNF"

decodeComp =
    decode Competition
        |> required "id" D.string
        |> required "name" D.string
        |> required "events" (D.list D.string)
        |> required "start" decodeDate
        |> required "people" (D.list decompCompetitor)

decodePerson =
    decode Person
        |> required "id" D.string
        |> requiredAt [ "person", "name" ] D.string
        |> requiredAt [ "person", "times" ] (D.dict <| D.list decodeTime)

decodeTime =
    D.oneOf
        [ D.float |> D.map Time
        , D.string 
            |> D.andThen 
                (\x -> 
                    case x of
                        "DNF" -> D.succeed DNF
                        _ -> D.fail "Wasn't DNF"
                )
        ]
decompCompetitor =
    decode Competitor
        |> required "id" D.string
        |> required "events" (D.list D.string)

decodeDate : D.Decoder Date.Date
decodeDate =
    D.string 
        |> D.andThen 
            (\st ->
                case Date.fromString st of
                    Ok date -> D.succeed date
                    Err e -> D.fail e
            )
