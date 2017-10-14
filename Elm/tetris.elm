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
    , paused : Bool
    }

type Msg
    = Update Time.Time
    | Key Int
    | SetDroppings (List Dropping)
    | SetDropping Dropping

init : (Model, Cmd Msg)
init = { lastTime = Nothing, paused = False, tetrisState = { blocks = defaultTetris, dropping = Nothing, nexts = [], hold = List.repeat 3 Nothing } } ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetDroppings d ->
            let state = model.tetrisState
            in
                { model
                | tetrisState = { state | nexts = state.nexts ++ d }
                } ! []
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
                40 -> -- Down
                    let (newState, cmd) = updateTetris 0 model.tetrisState
                    in
                        (
                            { model
                            | tetrisState = newState
                            }
                        , cmd
                        )
                27 -> -- Escape
                    { model | paused = not model.paused } ! []
                _ -> if code > 48 && code < 58  -- Is a number
                    then
                        let (newState, cmd) = holdPiece (code - 49) model.tetrisState
                        in ( { model | tetrisState = newState }, cmd)
                    else always (model ! []) <| Debug.log "Key down" code

        Update time ->
            if model.paused
                then model ! []
                else case model.lastTime of
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
    div []
    [ div [ id "gameS", align "center" ]
        [
        S.svg
            [ id "blur"
            , Sa.height <| toString <| size * List.length model.tetrisState.blocks
            , Sa.width <| toString <| size * width model.tetrisState.blocks
            ]
            <| renderTetris model.tetrisState
        ,
        S.svg
            [ id "game"
            , Sa.height <| toString <| size * List.length model.tetrisState.blocks
            , Sa.width <| toString <| size * width model.tetrisState.blocks
            ]
            <| renderTetris model.tetrisState
        ]
    , div [ id "hold" ]
        <| List.indexedMap
            (\i hold ->
                case hold of
                    Nothing ->
                        div [] 
                            [ div [ class "holdIdx" ]
                                [ text <| (toString <| i + 1) ++ ": " ]
                            , br [] []
                            ]
                    Just hold ->
                        div []
                            [ div [ class "holdIdx", class "active" ] [ text <| (toString <| i + 1) ++ ": " ]
                            , S.svg 
                                [ Sa.height <| toString <| size * List.length hold.shape
                                , Sa.width <| toString <| size * width hold.shape
                                ] <| renderGrid 0 0 hold.shape
                            ]
            )
            model.tetrisState.hold
    ]

subs model =
    Sub.batch
        [ Time.every (Time.second / 3) Update
        , Keyboard.downs Key
        ]


type BlockState
    = Empty
    | Filled FilledBlockState Color
    | Split FilledBlockState Color Color

isFilled : BlockState -> Bool
isFilled b = case b of
    Filled Full _ -> True
    Split _ _ _ -> True
    _ -> False

type FilledBlockState
    = DownLeft
    | DownRight
    | UpLeft
    | UpRight
    | Full

type Color
    = Red
    | Blue
    | Green
    | White
    | Yellow
    | LBlue
    | Orange
    | Purple

cols = [Red, Blue, Green, White, LBlue, Orange, Purple]

colStr c =
    case c of
        Red -> "red"
        Blue -> "rgb(0,0,255)"
        Green -> "rgb(0,255,0)"
        White -> "rgb(255,255,255)"
        Yellow -> "rgb(255,255,0)"
        LBlue -> "rgb(0,255,255)"
        Orange -> "rgb(255,120,0)"
        Purple -> "rgb(255,0,200)"

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
    , hold : List (Maybe Dropping)
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

merge : BlockState -> BlockState -> Maybe BlockState
merge a b =
    case (a, b) of
        (x, Empty) -> Just x
        (Empty, x) -> Just x
        (Filled DownLeft c, Filled UpRight c2) -> Just <| Split DownLeft c c2
        (Filled UpRight c, Filled DownLeft c2) -> Just <| Split DownLeft c2 c
        (Filled DownRight c, Filled UpLeft c2) -> Just <| Split DownRight c c2
        (Filled UpLeft c, Filled DownRight c2) -> Just <| Split DownRight c2 c
        _ -> Nothing

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

holdPiece : Int -> TetrisState -> (TetrisState, Cmd Msg)
holdPiece idx state =
    case (getAt idx state.hold, state.dropping) of
        (Just piece, Just dropping) ->
            { state
            | hold =
                Debug.log "Hold" <|
                    setAt state.hold idx <| Just dropping
            , dropping = Nothing
            } !
            case piece of
                Just piece ->
                    [ Random.generate SetDropping <| setPos state piece ]
                Nothing -> []
        a -> (always <| state ! []) <| Debug.log "Nope" a

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
        <| List.any
            <| not << isFilled

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
                    case merge p <|| getAt x bLine of
                        Just merged ->
                            let placed = setAt board y (setAt bLine x merged)
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
                            rotatePiece <|
                            Maybe.withDefault Empty <|
                            getAt x <||
                            getAt (List.length piece.shape - y - 1) piece.shape
                        )
                        <| List.range 0 <| List.length piece.shape - 1
                )
                <| List.range 0 <| width piece.shape - 1
    in { piece | shape = rotated }

rotatePiece : BlockState -> BlockState
rotatePiece p =
    case p of
        Empty -> Empty
        Filled x c -> Filled (rotateFill x) c
        Split x c1 c2 -> Split (rotateFill x) c1 c2

rotateFill x =
    case x of
        Full -> Full
        DownRight -> DownLeft
        DownLeft -> UpLeft
        UpLeft -> UpRight
        UpRight -> DownRight


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
            List.concat <|
                List.indexedMap (\x blk ->
                    renderBlock (y + oy) (x + ox) blk
                )
                line
        )

renderBlock : Int -> Int -> BlockState -> List (S.Svg msg)
renderBlock yp xp blk =
    let y = yp * size
        x = xp * size
    in case blk of
        Filled Full col ->
                [ S.rect
                    [ Sa.x <| toString x
                    , Sa.y <| toString y
                    , Sa.width <| toString size
                    , Sa.height <| toString size
                    , Sa.style <| "fill:" ++ colStr col
                    ] []
                ]
        Filled DownLeft col ->
                [ S.polygon
                    [ Sa.points <|
                        String.join " "
                        [ toString x ++ "," ++ toString y
                        , toString x ++ "," ++ toString (y + size)
                        , toString (x + size) ++ "," ++ toString (y + size)
                        ]
                    , Sa.style <| "fill:" ++ colStr col
                    ] []
                ]
        Filled DownRight col ->
                [ S.polygon
                    [ Sa.points <|
                        String.join " "
                        [ toString (x + size) ++ "," ++ toString y
                        , toString x ++ "," ++ toString (y + size)
                        , toString (x + size) ++ "," ++ toString (y + size)
                        ]
                    , Sa.style <| "fill:" ++ colStr col
                    ] []
                ]
        Filled UpLeft col ->
                [ S.polygon
                    [ Sa.points <|
                        String.join " "
                        [ toString x ++ "," ++ toString y
                        , toString x ++ "," ++ toString (y + size)
                        , toString (x + size) ++ "," ++ toString y
                        ]
                    , Sa.style <| "fill:" ++ colStr col
                    ] []
                ]
        Filled UpRight col ->
                [ S.polygon
                    [ Sa.points <|
                        String.join " "
                        [ toString x ++ "," ++ toString y
                        , toString (x + size) ++ "," ++ toString y
                        , toString (x + size) ++ "," ++ toString (y + size)
                        ]
                    , Sa.style <| "fill:" ++ colStr col
                    ] []
                ]
        Split shape c1 c2 ->
            let first  = renderBlock yp xp ( Filled shape c1 )
                second = renderBlock yp xp ( Filled (rotateFill <| rotateFill shape) c2 )
            in first ++ second
        _ ->
                [ S.rect
                    [ Sa.x <| toString x
                    , Sa.y <| toString y
                    , Sa.width <| toString size
                    , Sa.height <| toString size
                    , Sa.style "stroke-width:0.3;stroke:white"
                    , Sa.fillOpacity "0"
                    ] []
                ]

-- Generate a piece with the area of the argument. One area unit is the same as half a square.
generatePieceWithArea : Int -> Random.Generator Dropping
generatePieceWithArea area =
    Random.andThen randomRot
    <| case area of
        1 -> Re.constant ( Dropping 0 0 [ [ Filled DownRight Purple ] ] )
        _ ->
            let prev = generatePieceWithArea ( area - 1 )
                adder = Re.choices [ Random.andThen addTri prev, Random.andThen fillTri prev ]
            in adder


-- Helper for generatePieceWithArea. Adds one random triangle to a piece
addTri : Dropping -> Random.Generator Dropping
addTri piece =
    let fullPlaces =
            List.concat <|
                List.indexedMap
                    (\y line ->
                        let line = Maybe.withDefault [] <| getAt y piece.shape
                        in
                            List.filterMap
                            (\x ->
                                case getAt x line of
                                    Just (Filled Full _) -> Just (y, x)
                                    _ -> Nothing
                            ) <| List.range 0 <| List.length line - 1
                    ) piece.shape
        chooser =
            Re.sample fullPlaces
    in
        Random.andThen
            (\p -> case p of
                Nothing -> fillTri piece
                Just (y, x) ->
                    let shape =
                            if y == 0
                                then (List.map (always Empty) <| Maybe.withDefault [] <| List.head piece.shape)
                                    :: piece.shape
                                else piece.shape
                        yPlace =
                            if y == 0
                                then 0
                                else y - 1
                        at = getAt x <|| getAt yPlace shape
                        newPieceGen =
                            Random.map (\x -> Filled x Purple)
                                <| Re.choice DownRight DownLeft
                    in case at of
                        Just (Filled _ _) -> fillTri piece
                        _ -> Random.andThen
                                (\newPiece ->
                                    let newShape = Debug.log "newShape" <|
                                            setBlock yPlace x newPiece shape
                                    in case newShape of
                                        Nothing -> fillTri piece
                                        Just shape -> Re.constant
                                            { piece
                                            | shape = shape }
                                )
                                newPieceGen
            )
        chooser

-- Also a helper for generatePieceWithArea. Fills a random triangle to a square
fillTri : Dropping -> Random.Generator Dropping
fillTri piece =
    let halfPlaces =
            List.concat <|
                List.indexedMap
                    (\y line ->
                        let line = Maybe.withDefault [] <| getAt y piece.shape
                        in
                            List.filterMap
                            (\x ->
                                case getAt x line of
                                    Just (Filled Full _) -> Nothing
                                    Just (Filled _ _) -> Just (y, x)
                                    _ -> Nothing
                            ) <| List.range 0 <| List.length line - 1
                    ) piece.shape
        chooser =
            Re.sample halfPlaces
    in
        Random.andThen
            (\p -> case p of
                Nothing -> addTri piece
                Just (y, x) ->
                    let newShape =
                            setBlock y x (Filled Full Purple) piece.shape
                    in case newShape of
                        Nothing -> addTri piece
                        Just shape -> Re.constant
                            { piece
                            | shape = shape }
            )
        chooser


randomRot : Dropping -> Random.Generator Dropping
randomRot x =
    Random.map (Maybe.withDefault x)
        <| Re.sample
            [ x
            , rotate x
            , rotate <| rotate x
            , rotate <| rotate <| rotate x
            ]

nextsGenerator : TetrisState -> Random.Generator (List Dropping)
nextsGenerator state =
    Random.andThen
        Rl.shuffle
        <| Re.combine
        <| List.map
            (always <| Random.andThen (setProps state) <| generatePieceWithArea 7)
            (List.range 0 5)

randCol : Blocks -> Random.Generator Blocks
randCol shape =
    let col =
            Random.map (Maybe.withDefault Purple)
                <| Re.sample cols
    in Random.map
            (\col ->
                List.map
                    ( List.map
                        (\p -> case p of
                            Empty -> Empty
                            Filled t _ -> Filled t col
                            Split t _ _ -> Filled t col
                        )
                    )
                    shape
            )
            col



setPos : TetrisState -> Dropping -> Random.Generator Dropping
setPos state piece =
    let x = Random.int 0 <| width state.blocks - width piece.shape - 1
    in Random.map
            (\x ->
                { piece
                | x = x
                , y = 0
                }
            )
            x

setProps : TetrisState -> Dropping -> Random.Generator Dropping
setProps state piece =
    Random.andThen
        (\piece ->
            Random.map (\shape -> {piece | shape = shape})
            <| randCol piece.shape
        )
    <| setPos state piece

droppingGenerator : TetrisState -> Blocks -> Random.Generator Dropping
droppingGenerator state block =
    Random.andThen randomRot
    <| Random.map3
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