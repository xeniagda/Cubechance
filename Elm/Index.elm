import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Date

import Http

import Base

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
    }

init = update LoadUpcoming <| { competitions = [], search = "", error = Nothing }

type Msg
    = LoadUpcoming
    | ParseUpcoming (Result Http.Error String)
    | Search String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
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
        , renderComps <| List.sortBy (\comp -> Date.toTime comp.date) <| getMatchingComps model
        ] ++ (
            case model.error of
                Just err -> [p [ style [("color", "red")] ] [ text err ]]
                Nothing -> []
        )

renderComps : List Base.Competition -> Html Msg
renderComps comps =
    table [] <|
        List.map (\comp ->
            tr []
            [ a [href <| "/Comp.html?" ++ comp.id] [ td [] [text comp.name] ]
            , td [] [text comp.id]
            , td [] [text <| toString comp.date]
            ]
        ) comps


subs model = Sub.none