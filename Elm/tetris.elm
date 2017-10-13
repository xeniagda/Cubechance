port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg as S
import Svg.Attributes as Sa
import Time
import Keyboard
import Random
import Random.Extra as Re
import Random.List as Rl

import Base


size = 24

main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }

type alias Model =
    { lastTime : Maybe Time.Time
    , tetrisState : TetrisState
    }

type Msg
    = Update Time.Time
    | Key Int
    | SetDroppings (List Dropping)

init : (Model, Cmd Msg)
init = { lastTime = Nothing, tetrisState = { blocks = defaultTetris, dropping = Nothing, nexts = [] } } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetDroppings d ->
            let state = model.tetrisState
            in 
                { model 
                | tetrisState = { state | nexts = d }
                } ! []
        Key code ->
            case code of
                37 -> -- Left
                    { model | tetrisState = move model.tetrisState DLeft } ! []
                39 -> -- Right
                    { model | tetrisState = move model.tetrisState DRight } ! []
                38 -> -- Up
                    { model | tetrisState = rotate_ model.tetrisState } ! []
                40 -> -- Down
                    let (newState, cmd) = updateTetris 0 model.tetrisState
                    in 
                        (
                            { model
                            | tetrisState = newState
                            }
                        , cmd
                        )
                _ -> always (model ! []) <| Debug.log "Key down" code
        Update time -> 
            case model.lastTime of
                Nothing -> { model | lastTime = Just time } ! []
                Just last ->
                    let delta = (time - last) / 1000
                        (newState, cmd) = updateTetris delta model.tetrisState
                    in 
                        (
                            { model
                            | lastTime = Just time 
                            , tetrisState = newState
                            }
                        , cmd
                        )
view : Model -> Html Msg
view model =
    div [ id "game", align "center" ] 
        [ 
        S.svg
            [ id "gameS"
            , Sa.height <| toString <| size * List.length model.tetrisState.blocks
            , Sa.width <| toString <| size * width model.tetrisState.blocks
            ]
            <| renderTetris model.tetrisState
        ,
        S.svg 
            [ id "blur"
            , Sa.height <| toString <| size * List.length model.tetrisState.blocks
            , Sa.width <| toString <| size * width model.tetrisState.blocks
            ]
            <| renderTetris model.tetrisState
        ]

subs model = 
    Sub.batch
        [ Time.every (Time.second / 3) Update
        , Keyboard.downs Key
        ]


type BlockState 
    = Empty
    | Filled FilledBlockState Color

type Color
    = Red
    | Blue
    | Green
    | White
    | Yellow
    | LBlue
    | Orange

colStr c =
    case c of
        Red -> "red"
        Blue -> "rgb(0,0,255)"
        Green -> "rgb(0,255,0)"
        White -> "rgb(255,255,255)"
        Yellow -> "rgb(255,255,0)"
        LBlue -> "rgb(0,255,255)"
        Orange -> "rgb(255,120,0)"

type FilledBlockState
    = DownLeft
    | DownRight
    | UpLeft
    | UpRight
    | Full

type alias Blocks = List (List BlockState)

type alias Dropping =
    { y : Int
    , x : Int
    , shape : Blocks
    }

type alias TetrisState = 
    { blocks : Blocks
    , dropping : Maybe Dropping
    , nexts : List Dropping
    }


width : List (List a) -> Int
width x = List.length <| Maybe.withDefault [] <| List.head x

getAt : Int -> List a -> Maybe a
getAt n l = case (l, n) of
    ([], _) -> Nothing
    ((x::_), 0) -> Just x
    ((_::xs), _) -> getAt ( n - 1 ) xs

setAt : List a -> Int -> a -> List a
setAt l i a = case (l, i, a) of
    ((x::xs), 0, _) -> a :: xs
    ([], _, _) -> []
    ((x::xs), _, _) -> x :: setAt xs ( i - 1 ) a

setBlock : Int -> Int -> BlockState -> Blocks -> Maybe Blocks
setBlock y x block blocks =
    case getAt y blocks of
        Nothing -> Nothing
        Just line ->
            case getAt x line of
                Nothing -> Nothing
                Just _ ->
                    Just <|
                        setAt blocks y <|
                            setAt line x block

mmap : (a -> b) -> Maybe a -> Maybe b
mmap f m =
    case m of
        Nothing -> Nothing
        Just a -> Just <| f a
(<||) : (a -> Maybe b) -> Maybe a -> Maybe b
(<||) f m =
    case m of
        Nothing -> Nothing
        Just a -> f a
infixr 9 <||

defaultTetris =
    List.repeat 32 <|
    List.repeat 10 Empty

updateTetris : Float -> TetrisState -> (TetrisState, Cmd Msg)
updateTetris delta state =
    case state.dropping of
        Nothing ->
            case state.nexts of
                (p::rest) ->
                    { state
                    | dropping = Just p
                    , nexts = rest
                    } ! []
                [] ->
                    state ! [ Random.generate SetDroppings <| nextsGenerator state ]
        Just dropping ->
            let newDropping = { dropping | y = dropping.y + 1 }
            in
                if fits state.blocks newDropping
                    then 
                        { state
                        | dropping = Just newDropping
                        } ! []
                    else
                        { state
                        | dropping = Nothing
                        , blocks = 
                            removeWholeLines
                            <| Maybe.withDefault state.blocks
                            <| place state.blocks dropping
                        } ! []

removeWholeLines : Blocks -> Blocks
removeWholeLines =
    List.filter 
        <| not << 
            List.all (\k -> k /= Empty)

fits : Blocks -> Dropping -> Bool
fits board piece =
    place board piece /= Nothing

place : Blocks -> Dropping -> Maybe Blocks
place board piece =
    case piece.shape of
        [] -> Just board
        (line :: lines) ->
            case placeLine board piece.y piece.x line of
                Nothing -> Nothing
                Just board_ -> place board_ { piece | shape = lines, y = piece.y + 1 }

placeLine : Blocks -> Int -> Int -> List BlockState -> Maybe Blocks
placeLine board y x line =
    case line of
        [] -> Just board
        (Empty :: rest) ->
            placeLine board y (x + 1) rest
        (p :: rest) ->
            case getAt y board of
                Nothing -> Nothing
                Just bLine ->
                    case getAt x bLine of
                        Just Empty ->
                            let placed = setAt board y (setAt bLine x p)
                            in placeLine placed y (x + 1) rest
                        _ -> Nothing

type Dir
    = DLeft
    | DRight

move : TetrisState -> Dir -> TetrisState
move state d =
    case state.dropping of
        Just dropping ->
            let dir = if d == DLeft then -1 else 1
                newDropping = { dropping | x = dropping.x + dir }
            in if fits state.blocks newDropping
                then { state | dropping = Just newDropping }
                else state
        Nothing -> state

rotate_ : TetrisState -> TetrisState
rotate_ state =
    case state.dropping of
        Just dropping ->
            let rotated = rotate dropping
            in if fits state.blocks rotated
                  then { state | dropping = Just <| rotate dropping }
                  else state
        Nothing -> state

rotate : Dropping -> Dropping
rotate piece =
    let rotated =
            List.map
                (\x ->
                    List.map
                        (\y ->
                            Maybe.withDefault Empty <|
                            getAt x <||
                            getAt (List.length piece.shape - y - 1) piece.shape
                        )
                        <| List.range 0 <| List.length piece.shape - 1
                )
                <| List.range 0 <| width piece.shape - 1
    in { piece | shape = rotated }

renderTetris : TetrisState -> List (S.Svg msg)
renderTetris state =
    let placed =
            case state.dropping of
                Nothing -> state.blocks
                Just dropping -> Maybe.withDefault state.blocks <| place state.blocks dropping
    in renderGrid 0 0 placed

renderGrid oy ox =
    List.concat <<
        List.indexedMap (\y line ->
            List.indexedMap (\x blk ->
                renderBlock (y + oy) (x + ox) blk
            )
            line
        )

renderBlock : Int -> Int -> BlockState -> S.Svg msg
renderBlock y x blk =
    case blk of
        Filled _ col -> S.rect
                [ Sa.x <| toString <| x * size
                , Sa.y <| toString <| y * size
                , Sa.width <| toString size
                , Sa.height <| toString size
                , Sa.style <| "fill:" ++ colStr col
                ] []
        _ -> S.rect
                [ Sa.x <| toString <| x * size
                , Sa.y <| toString <| y * size
                , Sa.width <| toString size
                , Sa.height <| toString size
                , Sa.style "stroke-width:0.3;stroke:white"
                , Sa.fillOpacity "0"
                ] []

nextsGenerator : TetrisState -> Random.Generator (List Dropping)
nextsGenerator state =
    Random.andThen
        Rl.shuffle
        <| Re.combine
        <| List.map
            (droppingGenerator state)
            pieces


droppingGenerator : TetrisState -> Blocks -> Random.Generator Dropping
droppingGenerator state block =
    Random.map3
        Dropping
        (rConst 0)
        (Random.int 3 <| width state.blocks - 3)
        (rConst block)

rConst x = Random.map (always x) Random.bool


pieces =
    [ -- O
        [ [ Filled Full Red, Filled Full Red ], [ Filled Full Red, Filled Full Red ] ]
    , -- I
        [ [ Filled Full Blue, Filled Full Blue, Filled Full Blue, Filled Full Blue ] ]
    , -- T
        [ [ Empty, Filled Full Green, Empty ], [ Filled Full Green, Filled Full Green, Filled Full Green ] ]
    , -- S
        [ [ Empty, Filled Full LBlue, Filled Full LBlue ], [ Filled Full LBlue, Filled Full LBlue, Empty ] ]
    , -- Z
        [ [ Filled Full Orange, Filled Full Orange, Empty ], [ Empty, Filled Full Orange, Filled Full Orange ]]
    , -- J
        [ [ Filled Full White, Empty, Empty ], [ Filled Full White, Filled Full White, Filled Full White ] ]
    , -- L
        [ [ Filled Full Yellow, Filled Full Yellow, Filled Full Yellow ], [ Filled Full Yellow, Empty, Empty ] ]
    ]