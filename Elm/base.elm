module Base exposing (..)

import Dict exposing (..)
import Date
import Date.Extra

import Json.Decode as D
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional, hardcoded)


type alias Competition =
    { id : String
    , name : String
    , events : List String
    , date : Date.Date
    , competitors : List Competitor
    , country_iso : String
    , country_name : String
    , city : String
    }

type alias Competitor =
    { id : String
    , name : String
    , events : List String
    }

type alias Person =
    { id : String
    , name : String
    , times : Dict String (List Time) -- Event: [Time]
    , avgs : Dict String Time -- Event: avg
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
        |> required "competitors" (D.list decompCompetitor)
        |> required "country_iso" D.string
        |> required "country_name" D.string
        |> required "city" D.string

decodePerson =
    decode Person
        |> required "id" D.string
        |> requiredAt [ "person", "name" ] D.string
        |> requiredAt [ "person", "times" ] (D.dict <| D.list decodeTime)
        |> required "current_avgs" (D.dict <| D.index 0 decodeTime)

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
        |> required "name" D.string
        |> required "events" (D.list D.string)

decodeDate : D.Decoder Date.Date
decodeDate =
    D.string
        |> D.andThen
            (\st ->
                case Date.Extra.fromIsoString <| String.dropRight 3 st of
                    Just date -> D.succeed date
                    Nothing -> D.fail <| "Can't parse date " ++ toString st
            )
