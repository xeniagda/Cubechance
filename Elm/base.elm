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

viewDate : Date.Date -> String
viewDate date =
    (toString <| Date.dayOfWeek date)
    ++ ", " ++
    (toString <| Date.month date)
    ++ " " ++
    (toString <| Date.day date)
    ++ " " ++
    (toString <| Date.year date)

viewTime : Time -> String
viewTime x =
    case x of
        Time y -> viewTime_ y
        DNF -> "DNF"

viewTime_ : Float -> String
viewTime_ x =
    let hour =
            if x >= 3600
                then sti2 (floor x // 3600) ++ ":"
                else ""
        min =
            if x >= 60
                then sti2 ((floor x // 60) % 60) ++ ":"
                else ""
        second_ =  (stf2 <| fmod x 60)
        second = if x >= 60 && fmod x 60 < 10
                    then "0" ++ second_
                    else second_
    in hour ++ min ++ second

fmod : Float -> Float -> Float
fmod x y =
    x - y * (toFloat <| floor <| x / y)

sti2 : Int -> String -- Int to string with 2 decimals
sti2 x =
    let base = toString x
    in (String.repeat (2 - String.length base) "0") ++ base

stf2 : Float -> String -- Float to string with 2 decimals
stf2 x =
    let base = toString <| floor <| x * 100
        base_ = (String.repeat (3 - String.length base) "0") ++ base
    in String.dropRight 2 base_
    ++ "." ++ String.dropLeft (String.length base_ - 2) base_

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
