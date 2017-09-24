module Base exposing (..)

import Dict exposing (..)
import Date

import Json.Decode as D
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


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
    , times : Dict String Time -- Event: [Time]
    }

type Time 
    = Time Float
    | DNF

decodeComp =
    decode Competition
        |> required "id" D.string
        |> required "name" D.string
        |> required "events" (D.list D.string)
        |> required "start" decodeDate
        |> required "people" (D.list decompCompetitor)

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
