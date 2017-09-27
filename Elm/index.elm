import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Date
import Time

import Http

import Base

defaultSort = ( "Date", (\c1 c2 -> compare (Date.toTime c1.date) (Date.toTime c2.date)) )

sortings : List (String, (Base.Competition -> Base.Competition -> Order))
sortings =
    [ defaultSort
    , ( "Name", (\c1 c2 -> compare c1.name c2.name) )
    , ( "Number of people", (\c1 c2 -> compare (List.length c2.competitors) (List.length c1.competitors)) )
    , ( "Number of events", (\c1 c2 -> compare (List.length c2.events) (List.length c1.events)) )
    ]


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }

type alias Model =
    { competitions : List Base.Competition
    , search : String
    , error : Maybe String
    , sorting : (String, (Base.Competition -> Base.Competition -> Order))
    }

init =
    update LoadUpcoming <|
        { competitions = []
        , search = ""
        , error = Nothing
        , sorting = defaultSort
        }

type Msg
    = LoadUpcoming
    | ParseUpcoming (Result Http.Error String)
    | Search String
    | SetSorting String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetSorting name ->
            let sorting = 
                    List.head <|
                    List.filter (\sort -> Tuple.first sort == name)
                    sortings
            in case sorting of
                Nothing -> model ! []
                Just sorting ->
                    { model | sorting = sorting } ! []
        LoadUpcoming ->
            model !
            [
                Http.send ParseUpcoming
                    <| Http.getString "api/upcoming"
            ]

        ParseUpcoming (Ok text) ->
            case D.decodeString (D.list Base.decodeComp) text of
                Ok comps ->
                    { model |
                        competitions = comps
                    } ! []
                Err err ->
                    { model | error = Just <| toString err } ! []

        ParseUpcoming (Err err) ->
            { model | error = Just <| toString err } ! []

        Search st -> { model | search = st } ! []

getMatchingComps : Model -> List Base.Competition
getMatchingComps { search, competitions } =
    List.filter
        (\comp ->
            String.contains
                (String.toLower search)
                (String.toLower comp.name)
        )
        competitions

view : Model -> Html Msg
view model =
    div [] <|
        [ input [ placeholder "Search", value model.search, onInput Search ] []
        , viewDropdown model
        , renderComps <| List.sortWith (Tuple.second model.sorting) <| getMatchingComps model
        ] ++ (
            case model.error of
                Just err -> [p [ style [("color", "red")] ] [ text err ]]
                Nothing -> []
        )

viewDropdown : Model -> Html Msg
viewDropdown model =
    select [ onInput SetSorting ] <|
        List.map
            (\(name, _) ->
                option [ selected <| name == Tuple.first model.sorting ] 
                    [ text name ]
            )
        sortings


renderComps : List Base.Competition -> Html Msg
renderComps comps =
    table [] <|
        List.map (\comp ->
            tr []
            [ a [href <| "/comp.html?" ++ comp.id] [ td [] [text comp.name] ]
            , td [] [text comp.id]
            , td [] [text <| Base.viewDate comp.date]
            ]
        ) comps


subs model = Time.every (Time.second * 10) <| always LoadUpcoming