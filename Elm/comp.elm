port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Date
import Dict
import Time
import Task
import Ordinal exposing (ordinal)

import Http

import Base

usage =
    "Click on a persons name to select that person, then click on an event to see how well that person would do in that event. Displayed on each event is the corresponding persons average in that event. To add a person that's not registered, enter the WCA ID or the name of the person in the \"Add person\" field. Click on an icon with an event to sort every person according to their time in that event."


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
    , matching : List Base.Person
    , search : String
    , help : Bool
    }

type Selected
    = SelectEvent Base.Person
    | Waiting Base.Person String
    | Loaded Base.Person String (List Float)

type Msg
    = LoadComp
    | ParseComp (Result Http.Error String)
    | SelectedPerson Base.Person
    | SetOther String
    | SelectedOther
    | ParseOther (Result Http.Error String)
    | SelectedEvent String
    | ParseChances (Result Http.Error String)
    | SortBy (Maybe String)
    | Help

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
                , matching = []
                , search = ""
                , help = False
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
            case model.selected of
                Just (SelectEvent e) ->
                    { model | selected = Nothing, matching = [] } ! []
                _ ->
                    { model
                    | selected = Just <| SelectEvent p
                    , matching = []
                    } ! []

        SetOther name ->
            { model
            | search = name
            } ! []

        SelectedOther ->
            model !
            [ Http.send ParseOther
                <| Http.getString
                    <| "api/people/" ++ model.search
            ]

        ParseOther r ->
            case r of
                Ok res ->
                    case D.decodeString (D.list Base.decodePerson) res of
                        Ok matching ->
                            case matching of
                                [x] ->
                                    { model
                                    | selected = Just <| SelectEvent x
                                    } ! []
                                _ ->
                                    { model
                                    | matching = matching
                                    } ! []
                        Err e -> { model | error = Just <| toString e } ! []
                Err e -> { model | error = Just <| toString e } ! []

        SelectedEvent e ->
            case model.selected of
                Just (SelectEvent p) ->
                    { model
                    | selected = Just <| Waiting p e
                    , matching = []
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

        Help ->
            { model | help = not model.help } ! []

decodePlaces = D.list D.float

view model =
    let compLink = "https://www.worldcubeassociation.org/competitions/" ++ model.compId
    in div []
        [ a [ id "back", href "/index.html" ] [ text "←" ]
        , case model.comp of
            Just comp ->
                div [id "center"]
                    [ h1 [id "title"] [text comp.name]
                    , a [ id "compLink", href compLink ] [ text "(On WCA)" ]
                    , br [] []
                    , p []
                        [ span [ class <| "flag-icon flag-icon-" ++ String.toLower comp.country_iso ] []
                        , text " - "
                        , b [] [ text <| comp.country_name]
                        , text <| ", " ++ comp.city
                    ]
                    ]
            Nothing ->
                div [] []
        , br [] []
        , div [ style [("right", "0")] ]
            [ text "Add person: "
            , input [ placeholder "Name", value model.search, onInput SetOther ] []
            , button [ id "go", onClick SelectedOther ] [ text "Go!" ]
            ]
        , case model.matching of
            [] -> text ""
            _ ->
                table [] <| List.concat
                    [ [ tr [] [th [] [text "Name"], th [] [text "Wca ID"]] ]
                    , List.map (\p ->
                        tr []
                            [ td [ onClick <| SelectedPerson p ] [text p.name]
                            , td [] [text p.id]]
                        ) <| model.matching
                    , if List.length model.matching >= 20
                         then [ tr [] [ td [] [ text "..." ] ] ]
                         else [  ]
                    ]
        , case model.selected of
            Just (Loaded person event places) ->
                let placesWithIndexed =
                        List.sortBy Tuple.first <|
                        List.filter (\(_, chance) -> chance > 0.01) <|
                        List.take 10 <|
                        List.sortBy (negate << Tuple.second) <|
                        List.indexedMap (,) places
                in div []
                    [ p [] [ text <| person.name ++ " has the following chances in "
                                 , genIcon event
                                 , text ":"]
                    , table [ id "chances" ] <|
                        [ tr [] <|
                            List.map (\ (i, chance) ->
                                th [] [ text <| ordinal <| i + 1 ]
                            ) placesWithIndexed
                        , tr [] <|
                            List.map (\ (i, chance) ->
                                td [] [ text <| Base.stf2 (chance * 100) ++ "%" ]
                            ) placesWithIndexed
                        ]
                    ]
            _ -> text ""
        , case model.comp of
            Just comp ->
                div [ id "comp" ]
                [ case model.selected of
                    Just (SelectEvent per) -> viewCompetitors model.sortBy comp model.people <| Just per
                    _ ->                      viewCompetitors model.sortBy comp model.people Nothing
                , br [] []
                , hr [] []
                , a [ onClick Help
                    , style [ ("color", "blue") ]
                    ]
                    [ text <| if model.help then "Hide usage" else "How to use" ]
                , if model.help
                     then p [] [ text usage ]
                     else div [] []
                ]
            _ ->
                div []
                [ br [] []
                , div [id "loadingcircle"] []
                ]
        ]

compareP people method c1 c2 =
    case ( findPerson c1.id people
         , findPerson c2.id people) of
        (Just p1, Just p2) ->
            case method of
                Just event ->
                    case ( List.any (\a -> a == event) c1.events
                         , List.any (\a -> a == event) c2.events ) of
                        (False, _) -> GT
                        (_, False) -> LT
                        _ -> case ( Dict.get event p1.avgs
                                  , Dict.get event p2.avgs) of
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
    let competitors =
            List.filterMap
            (\competitor ->
                case findPerson competitor.id people of
                    Nothing -> Nothing
                    Just person -> Just <| viewCompetitor selected competition competitor person
            )
            (List.sortWith (compareP people sort) competition.competitors)
        person =
            case selected of
                Just x ->
                    if not <| List.any (\p -> x.id == p.id) competition.competitors
                        then [ viewCompetitor selected competition (Base.Competitor x.id x.name <| Dict.keys x.times) x ]
                        else [  ]
                _ -> []
    in table [id "list"]
        <| genHeader competition
        :: person
        ++ competitors
        ++ [ genFooter competition ]

viewCompetitor select competition competitor person =
    let personLink = "https://www.worldcubeassociation.org/persons/" ++ person.id
        tClass =
            case select of
                Just x ->
                    if x.id == person.id
                       then "competitor selected"
                       else "competitor deselected"
                _ -> "competitor"
    in tr [class tClass]
        <|
        [ td [class "comp_name"] [
            a [ onClick <| SelectedPerson person ] [ text person.name]
        ]
        ] ++ List.map (\event -> displayEvent select event competitor person) competition.events

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

genFooter competition =
    tr [class "comp-events" ] <|
        th [ class "total", onClick <| SortBy Nothing ] [ text "# Competitors" ]
     --:: th [ class "comp-id"] [ text competition.id ]
     :: List.map
            (\event ->
                th [ class "number", onClick <| SortBy (Just event) ]
                [ span [class <| "cubing-icon event-" ++ event ] []
                , text <| ": " ++
                    ( toString <| List.length <|
                        List.filter (\c -> List.member event c.events) competition.competitors
                    )
                ]
            )
            competition.events

displayEvent select event competitor person =
    let isSelected =
            case select of
                Just x -> x.id == person.id
                _ -> False
    in case Dict.get event person.avgs of
        Nothing -> td [class "event"] []
        Just avg ->
            if List.any (\a -> a == event) competitor.events then
                let click =
                        case select of
                            Just _ -> [ onClick <| SelectedEvent event ]
                            _ -> []
                in td ([class "event"] ++ click)
                    [ text <| Base.viewTime avg ]
            else td [class "event"] []

genIcon event =
    span [ class <| "cubing-icon event-" ++ event ] []



findPerson : String -> List Base.Person -> Maybe Base.Person
findPerson id people =
    List.head
        <| List.filter (\person -> person.id == id) people

subs model = Time.every (Time.second * 10) <| always LoadComp
