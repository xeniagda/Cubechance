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
    , searchCountry : String
    , sorting : (String, (Base.Competition -> Base.Competition -> Order))
    , serverLoading : Bool
    }

init =
    update LoadUpcoming <|
        { competitions = []
        , search = ""
        , searchPerson = ""
        , searchCountry = ""
        , sorting = defaultSort
        , serverLoading = False
        }

type Msg
    = LoadUpcoming
    | ParseUpcoming (Result Http.Error String)
    | Search String
    | SearchPerson String
    | SearchCountry String
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
            case model.competitions of
                [] ->
                    model !
                    [ Http.send ParseUpcoming
                        <| Http.getString "api/upcoming"
                    ]
                _ -> model ! []

        ParseUpcoming (Ok text) ->
            if text == "e0"
                then { model | serverLoading = True } ! []
                else case D.decodeString (D.list Base.decodeComp) text of
                    Ok comps ->
                        { model
                        | competitions = Debug.log "Comps" comps
                        , serverLoading = False
                        } ! []
                    Err err ->
                        let _ = Debug.log "Server error" err
                        in model ! []

        ParseUpcoming (Err err) ->
            let _ = Debug.log "Server error" err
            in model ! []

        Search st -> { model | search = st } ! []

        SearchPerson st -> { model | searchPerson = st } ! []

        SearchCountry st -> { model | searchCountry = st } ! []

getMatchingComps : Model -> List Base.Competition
getMatchingComps { search, searchPerson, searchCountry, competitions } =
    if search == "" && searchPerson == "" && searchCountry == ""
        then competitions
        else List.filter
            (\comp ->
                (
                    String.contains
                        (String.toLower search)
                        (String.toLower comp.name)
                    && String.contains
                        (String.toLower searchCountry)
                        (String.toLower <| comp.country_name ++ "\0" ++ comp.country_iso ++ "\0" ++ comp.city)
                )
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
    div []
     <| pageTitle model
     :: [ genSearch model
        , renderComps <| List.sortWith (Tuple.second model.sorting) <| getMatchingComps model
        , wcaDisc
        , if model.serverLoading
             then p [ id "loading" ] [ text "The server is currently loading the results from WCA. This usually takes around one minute." ]
             else div [] []
        , if model.competitions == []
             then div [ id "loadingcircle" ] []
             else div [] []
        , p [] [ text "Other links: " ]
        , ul [] [
            a [ href "tetris.html" ] [ text "Trekantris" ]
            ]
        ]

pageTitle : Model -> Html Msg
pageTitle model =
    div []
        [ h1 [ style
            [ ("text-align", "center")
            , ("padding-top", "30px")
            , ("padding-bottom", "30px")
            ] ] [ text "Cubechance" ]
        , hr [] []
        ]

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
                [ text "Search country/city: "
                , input [ placeholder "London, China, ect.", value model.searchCountry, onInput SearchCountry ] []
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
        , th [] [ text "Country" ]
        , th [] [ text "Date" ]
        ]
     :: List.map (\comp ->
            tr []
            [ a [href <| "/comp.html?" ++ comp.id] [ td [] [text comp.name] ]
            , td []
                [ span [ class <| "flag-icon flag-icon-" ++ String.toLower comp.country_iso ] []
                , text " - "
                , b [] [ text <| comp.country_name]
                , text <| ", " ++ comp.city
                ]
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


subs model = Time.every (Time.second * 5) <| always LoadUpcoming