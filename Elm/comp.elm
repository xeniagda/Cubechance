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
    , sortBy : Maybe String
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
    | SortBy (Maybe String)

init : Flags -> (Model, Cmd Msg)
init flags =
    let (model, cmd) = 
            update LoadComp <|
                { compId = flags.compId
                , comp = Nothing
                , people = []
                , selected = Nothing
                , error = Nothing
                , sortBy = Nothing
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

        SortBy x ->
            { model | sortBy = x } ! []

decodePlaces = D.list D.float

view model =
    let compLink = "https://www.worldcubeassociation.org/competitions/" ++ model.compId
    in div [] 
        [ a [ href "/index.html" ] [ text "←" ]
        , br [] []
        , case model.selected of
            Just (Loaded person event places) ->
                let placesWithIndexed =
                        List.indexedMap (\i place -> (i, place)) places
                in div [] <|
                    p [] [text <| person.name ++ " has the following chances in " ++ event ++ ":"]
                    :: List.filterMap (\ (i, chance) ->
                        if chance > 0.01
                           then Just <| p [] [ text <| toString i ++ ": " ++ Base.stf2 (chance * 100) ++ "%" ]
                           else Nothing
                    ) placesWithIndexed
            _ -> text ""
        , case model.comp of
            Just comp ->
                div []
                    [ div [id "center"] [
                        h1 [id "title"] [text comp.name]
                        , a [ id "compLink", href compLink ] [ text "(On WCA)" ]
                    ]
                    , case model.selected of
                        Just (SelectEvent per) ->
                            viewCompetitors model.sortBy comp model.people <| Just per.id
                        _ -> viewCompetitors model.sortBy comp model.people Nothing
                    ]
            _ ->
                p [id "loading"] [text "Loading..."]
        ]

compareP people method c1 c2 =
    case ( findPerson c1.id people
         , findPerson c2.id people) of
        (Just p1, Just p2) ->
            case method of
                Just event ->
                    case ( Maybe.map average <| Dict.get event p1.times
                         , Maybe.map average <| Dict.get event p2.times) of
                        ( Just (Base.Time t1)
                        , Just (Base.Time t2))  -> compare t1 t2
                        (Just Base.DNF, Just _) -> GT
                        (Just _, Just Base.DNF) -> LT
                        (Nothing, Just _)       -> GT
                        (Just _, Nothing)       -> LT
                        _                       -> EQ
                Nothing -> compare p1.name p2.name
        _ -> EQ

viewCompetitors sort competition people selected =
    table [id "competitors"]
        <| genHeader competition
        :: List.filterMap
            (\competitor ->
                case findPerson competitor.id people of
                    Nothing -> Nothing
                    Just person ->
                        case selected of
                            Just x ->
                                if x == competitor.id
                                   then Just <| viewCompetitor True competition competitor person
                                   else Nothing
                            Nothing -> Just <| viewCompetitor False competition competitor person
            )
            (List.sortWith (compareP people sort) competition.competitors)

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
        th [ class "name", onClick <| SortBy Nothing ] [ text "Name" ]
     --:: th [ class "comp-id"] [ text competition.id ]
     :: List.map
            (\event ->
                th [ class "event", onClick <| SortBy (Just event) ]
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
