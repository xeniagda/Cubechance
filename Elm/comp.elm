import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Date
import Dict

import Http

import Base

main =
    programWithFlags
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }

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
            [
                Http.send ParseComp
                    <| Http.getString 
                        <| "api/comp/" ++ model.compId
            ]

        ParseComp (Ok text) ->
            case D.decodeString decodeCompAndPeople text of
                Ok (comp, people) ->
                    { model 
                        | comp = Just comp
                        , people = people
                    } ! []
                Err err ->
                    { model | error = Just <| toString err } ! []
        ParseComp (Err err) ->
            { model | error = Just <| toString err } ! []

view model =
    case model.comp of
        Just comp ->
            div []
                [ h1 [id "title"] [text comp.name]
                , viewCompetitors comp model.people
                ]
        _ ->
            p [id "loading"] [text "Loading..."]

viewCompetitors comp people =
    table [id "competitors"]
        <| List.filterMap
            (\competitor ->
                case findPerson competitor.id people of
                    Nothing -> Nothing
                    Just person ->
                        Just <| viewCompetitor competitor person
            )
            comp.competitors

viewCompetitor comp person =
    tr [class "competitor"]
        <|
        [ td [class "comp_name"] [text person.name]
        , td [class "comp_id"] [text person.id]
        ] ++ List.map (\event -> displayEvent event comp person) comp.events

displayEvent event comp person =
    let times = Dict.get event person.times
    in case times of
        Just times ->
            td [class "event"]
                [ text <| event ++ ": " ++ ( Base.stringify <| average times )
                ]
        Nothing ->
            td [class "event"] [ text <| toString event]

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

subs model = Sub.none
