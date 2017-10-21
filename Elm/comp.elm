port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Date
import Dict
import Time
import Task

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
    , selected : Maybe Selected
    , error : Maybe String
    }

type Selected
    = SelectEvent Base.Person
    | Waiting Base.Person String
    | Loaded Base.Person String (List Float)

type Msg
    = LoadComp
    | ParseComp (Result Http.Error String)
    | SelectedPerson Base.Person
    | SelectedEvent String
    | ParseChances (Result Http.Error String)

init : Flags -> (Model, Cmd Msg)
init flags =
    let (model, cmd) = 
            update LoadComp <|
                { compId = flags.compId
                , comp = Nothing
                , people = []
                , selected = Nothing
                , error = Nothing
                }
    in (model, Cmd.batch <| [Task.perform SelectedEvent <| Task.succeed "333", cmd])

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
                    { model | error = Just <| Debug.log "Error" <| toString err } ! []

        SelectedPerson p ->
            { model
            | selected = Just <| SelectEvent p
            } ! []

        SelectedEvent e -> 
            case model.selected of
                Just (SelectEvent p) ->
                    { model
                    | selected = Just <| Waiting p e
                    } !
                    [ Http.send ParseChances
                        <| Http.getString
                            <| "api/place/" ++ model.compId ++ "/" ++ p.id ++ "/" ++ e
                    ]
                _ -> model ! []

        ParseChances res ->
            case (res, model.selected) of
                (Ok json, Just (Waiting p e)) ->
                    case D.decodeString decodePlaces json of
                        Ok places ->
                            { model | selected = Just <| Loaded p e places} ! []
                        Err e -> { model | error = Just <| toString e } ! []
                (Err e, _) -> { model | error = Just <| toString e } ! []
                _ -> model ! []

        ParseComp (Err err) ->
            { model | error = Just <| toString err } ! []
decodePlaces = D.list D.float

view model =
    let compLink = "https://www.worldcubeassociation.org/competitions/" ++ model.compId
    in div [] 
        [ a [ href "/index.html" ] [ text "←" ]
        , br [] []
        , p [] [ text <| toString model.selected ]
        , p [] [ text <| toString model.error ]
        , case model.comp of
            Just comp ->
                div []
                    [ div [id "center"] [
                        h1 [id "title"] [text comp.name]
                        , a [ id "compLink", href compLink ] [ text "(On WCA)" ]
                    ]
                    , case model.selected of
                        Just (SelectEvent per) ->
                            table [id "competitors"]
                                <| genHeader comp
                                :: List.filterMap
                                    (\competitor ->
                                        case findPerson competitor.id model.people of
                                            Nothing -> Nothing
                                            Just person ->
                                                if person.id == per.id
                                                   then Just <| viewCompetitor True comp competitor person
                                                   else Nothing
                                    ) comp.competitors
                        _ -> viewCompetitors comp model.people
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
                        Just <| viewCompetitor False competition competitor person
            )
            competition.competitors

viewCompetitor select competition competitor person =
    let personLink = "https://www.worldcubeassociation.org/persons/" ++ person.id
    in tr [class "competitor"]
        <|
        [ td [class "comp_name"] [
            a [ onClick <| SelectedPerson person ] [ text person.name]
        ]
        ] ++ List.map (\event -> displayEvent select event competition person) competition.events

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

displayEvent select event comp person =
    let times = Dict.get event person.times
    in case times of
            Nothing -> td [class "event"] []
            Just times ->
                let click =
                        if select
                            then [ onClick <| SelectedEvent event ]
                            else []
                in td ([class "event"] ++ click)
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
