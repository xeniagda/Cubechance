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
    , searchPerson : String
    , error : Maybe String
    , sorting : (String, (Base.Competition -> Base.Competition -> Order))
    }

init =
    update LoadUpcoming <|
        { competitions = []
        , search = ""
        , searchPerson = ""
        , error = Nothing
        , sorting = defaultSort
        }

type Msg
    = LoadUpcoming
    | ParseUpcoming (Result Http.Error String)
    | Search String
    | SearchPerson String
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

        SearchPerson st -> { model | searchPerson = st } ! []

getMatchingComps : Model -> List Base.Competition
getMatchingComps { search, searchPerson, competitions } =
    List.filter
        (\comp ->
            String.contains
                (String.toLower search)
                (String.toLower comp.name)
            && List.any 
                (\p ->
                    String.contains
                        (String.toLower searchPerson)
                        (String.toLower p.id)
                    || String.contains
                        (String.toLower searchPerson)
                        (String.toLower p.name)
                ) comp.competitors
        )
        competitions

view : Model -> Html Msg
view model =
    div [] <|
        [ genSearch model
        , renderComps <| List.sortWith (Tuple.second model.sorting) <| getMatchingComps model
        , wcaDisc
        ] ++ (
            case model.error of
                Just err -> [p [ style [("color", "red")] ] [ text err ]]
                Nothing -> []
        )

genSearch model =
    table [ id "search" ]
        [ tr [] 
            [ th [] 
                [ text "Search comp: "
                , input [ placeholder "Comp", value model.search, onInput Search ] []
                ]
            , th [] 
                [ text "Search person: "
                , input [ placeholder "WCA ID / Name", value model.searchPerson, onInput SearchPerson ] [] 
                ]
            , th [] 
                [ text "Sort by: "
                , viewDropdown model 
                ]
            ]
        ]

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
    table [ id "list" ] <|
    tr [] 
        [ th [] [ text "Competition" ]
        , th [] [ text "Competition ID" ]
        , th [] [ text "Date" ]
        ]
     :: List.map (\comp ->
            tr []
            [ a [href <| "/comp.html?" ++ comp.id] [ td [] [text comp.name] ]
            , td [] [text comp.id]
            , td [] [text <| Base.viewDate comp.date]
            ]
        ) comps

wcaDisc =
    p [style [( "font-size", "9pt" )]]
        [ text "Data taken from World Cube Association ("
        , a [ href "https://www.worldcubeassociation.org" ] [ text "https://www.worldcubeassociation.org" ]
        , text ") daily. This is not the actual information, it can be found "
        , a [ href "https://www.worldcubeassociation.org/results" ] [ text "here" ]
        , text ". "
        ]


subs model = Time.every (Time.second * 10) <| always LoadUpcoming