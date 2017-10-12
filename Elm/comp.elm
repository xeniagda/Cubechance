port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Date
import Dict
import Time

import Http

import Base

main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }

port title : String -> Cmd a

type alias Flags =
    { compId : String }

type alias Model =
    { compId : String
    , comp : Maybe Base.Competition
    , people : List Base.Person
    , error : Maybe String
    }

type Msg
    = LoadComp
    | ParseComp (Result Http.Error String)

init : Flags -> (Model, Cmd Msg)
init flags =
    update LoadComp <|
        { compId = flags.compId
        , comp = Nothing
        , people = []
        , error = Nothing
        }

decodeCompAndPeople =
    D.map2
        (\comp people -> (comp, people))
        (D.field "comp" Base.decodeComp)
        (D.field "people" <| D.list Base.decodePerson)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        LoadComp ->
            model !
            [ Http.send ParseComp
                <| Http.getString
                    <| "api/comp/" ++ model.compId
            ]

        ParseComp (Ok text) ->
            case D.decodeString decodeCompAndPeople text of
                Ok (comp, people) ->
                    { model
                        | comp = Just comp
                        , people = people
                    } ! 
                    [ title <| comp.name
                    ]
                Err err ->
                    { model | error = Just <| toString err } ! []
        ParseComp (Err err) ->
            { model | error = Just <| toString err } ! []

view model =
    let compLink = "https://www.worldcubeassociation.org/competitions/" ++ model.compId
    in div [] 
        [ a [ href "/index.html" ] [ text "←" ]
        , br [] []
        , case model.comp of
            Just comp ->
                div []
                    [ div [id "center"] [
                        h1 [id "title"] [text comp.name]
                        , a [ id "compLink", href compLink ] [ text "(On WCA)" ]
                    ]
                    , viewCompetitors comp model.people
                    ]
            _ ->
                p [id "loading"] [text "Loading..."]
        ]

viewCompetitors competition people =
    table [id "competitors"]
        <| genHeader competition
        :: List.filterMap
            (\competitor ->
                case findPerson competitor.id people of
                    Nothing -> Nothing
                    Just person ->
                        Just <| viewCompetitor competition competitor person
            )
            competition.competitors

viewCompetitor competition competitor person =
    let personLink = "https://www.worldcubeassociation.org/persons/" ++ person.id
    in tr [class "competitor"]
        <|
        [ td [class "comp_name"] [
            a [ href personLink ] [ text person.name]
        ]
        ] ++ List.map (\event -> displayEvent event competition person) competition.events

genHeader competition =
    tr [class "comp-events" ] <|
        th [ class "comp-name"] [ text competition.name ]
     --:: th [ class "comp-id"] [ text competition.id ]
     :: List.map
            (\event ->
                th [ class "event" ]
                [ span [class <| "cubing-icon event-" ++ event ] []
                ]
            )
            competition.events

displayEvent event comp person =
    let times = Dict.get event person.times
    in case times of
           Nothing -> td [class "event"] []
           Just times ->
               td [class "event"]
                   [ text <| Base.viewTime <| average times
                   ]

genIcon event =
    span [ class <| "cubing-icon event-" ++ event ] []

average : List Base.Time -> Base.Time
average times =
    let non_dnfs =
            List.filterMap
                (\x ->
                    case x of
                        Base.DNF -> Nothing
                        Base.Time n -> Just n
                )
            times
    in case non_dnfs of
        [] -> Base.DNF
        _ ->
            let avg = List.sum non_dnfs / toFloat (List.length non_dnfs)
            in Base.Time <| toFloat (round <| avg * 100) / 100



findPerson : String -> List Base.Person -> Maybe Base.Person
findPerson id people =
    List.head
        <| List.filter (\person -> person.id == id) people

subs model = Time.every (Time.second * 10) <| always LoadComp
