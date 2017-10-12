port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg as S
import Svg.Attributes as Sa
import Time
import Keyboard
import Random

import Base

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
    | SetDropping Dropping

init : (Model, Cmd Msg)
init = { lastTime = Nothing, tetrisState = { blocks = defaultTetris, dropping = Nothing } } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetDropping d ->
            let state = model.tetrisState
            in 
                { model 
                | tetrisState = { state | dropping = Just d } 
                } ! []
        Key code ->
            case code of
                37 -> -- Left
                    { model | tetrisState = move model.tetrisState DLeft } ! []
                39 -> -- Right
                    { model | tetrisState = move model.tetrisState DRight } ! []
                38 -> -- Up
                    { model | tetrisState = rotate_ model.tetrisState } ! []
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

view model =
    div [] [
        S.svg [Sa.width "500px", Sa.height "500px"] <| renderTetris model.tetrisState
        ]

subs model = 
    Sub.batch
        [ Time.every (Time.second / 3) Update
        , Keyboard.downs Key
        ]


type BlockState 
    = Empty
    | DownLeft
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

defaultDropping =
    { x = 1
    , y = 0
    , shape = [[ Full, Empty ], [ Full, Empty ], [ Full, Full ]]
    }

updateTetris : Float -> TetrisState -> (TetrisState, Cmd Msg)
updateTetris delta state =
    case state.dropping of
        Nothing -> state ! [ Random.generate SetDropping <| droppingGenerator state ]
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
                        } ! [ Random.generate SetDropping <| droppingGenerator state ]
removeWholeLines : Blocks -> Blocks
removeWholeLines =
    List.filter <| not << List.all (\k -> k == Full)

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
    in List.concat <|
        List.indexedMap (\y line ->
            List.indexedMap (\x blk ->
                renderBlock state y x blk
            )
            line
        )
        placed

renderBlock : TetrisState -> Int -> Int -> BlockState -> S.Svg msg
renderBlock state y x blk =
    case blk of
        Full -> S.rect
                [ Sa.x <| toString <| x * 10
                , Sa.y <| toString <| y * 10
                , Sa.width "10"
                , Sa.height "10"
                , Sa.style "fill:red"
                ] []
        _ -> S.rect
                [ Sa.x <| toString <| x * 10
                , Sa.y <| toString <| y * 10
                , Sa.width "10"
                , Sa.height "10"
                , Sa.style "fill:gray"
                ] []

droppingGenerator : TetrisState -> Random.Generator Dropping
droppingGenerator state =
    Random.map3
        Dropping
        (rConst 0)
        (Random.int 3 <| width state.blocks - 3)
        blockGenerator

blockGenerator : Random.Generator Blocks
blockGenerator =
    Random.andThen
    (\x ->
        case getAt x pieces of
            Just p -> rConst p
            Nothing -> blockGenerator
    )
    (Random.int 0 6)

rConst x = Random.map (always x) Random.bool


pieces =
    [ -- O
        [ [ Full, Full ], [ Full, Full ] ]
    , -- I
        [ [ Full, Full, Full, Full ] ]
    , -- T
        [ [ Empty, Full, Empty ], [ Full, Full, Full ] ]
    , -- S
        [ [ Empty, Full, Full ], [ Full, Full, Empty ] ]
    , -- Z
        [ [ Full, Full, Empty ], [ Empty, Full, Full ]]
    , -- J
        [ [ Full, Empty, Empty ], [ Full, Full, Full ] ]
    , -- L
        [ [ Full, Full, Full ], [ Full, Empty, Empty ] ]
    ]