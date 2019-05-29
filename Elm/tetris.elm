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

dropScore = 5
hashPrimeMod = 80953
hashPrimeMul = 54833
dims = (10, 32) -- width, height

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
    , settings : Settings
    , paused : Bool
    , changingSettings : Bool
    }

type alias Settings =
    { scale : Int
    , hard : Bool
    , pieceSize : Int
    }

init : (Model, Cmd Msg)
init =
    { lastTime = Nothing
    , paused = False
    , settings =
        { scale = 20
        , hard = False
        , pieceSize = 7
        }
    , tetrisState =
        { blocks = defaultTetris
        , dropping = Nothing
        , nexts = []
        , hold = List.repeat 3 Nothing
        , score = 0
        , gameOver = False
        }
    , changingSettings = True
    } ! []


type Msg
    = Update Time.Time
    | Key Int
    | SetDroppings (List Dropping)
    | SetDropping Dropping
    | Restart
    | SetSettings Settings
    | Started

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Restart ->
            let mod = Tuple.first init
            in { mod | settings = model.settings } ! []

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
                    if model.paused then model ! [] else { model | tetrisState = move model.tetrisState DLeft } ! []
                39 -> -- Right
                    if model.paused then model ! [] else { model | tetrisState = move model.tetrisState DRight } ! []
                38 -> -- Up
                    if model.paused then model ! [] else { model | tetrisState = rotate_ model.tetrisState } ! []
                40 -> -- Down
                    if model.paused then model ! [] else
                        let (newState, cmd) = updateTetris model.settings 0 model.tetrisState
                        in
                            (
                                { model
                                | tetrisState = newState
                                }
                            , cmd
                            )
                27 -> -- Escape
                    { model | paused = not model.paused } ! []
                32 -> -- Space
                    if model.paused
                        then model ! []
                        else { model | tetrisState = drop model.settings 0 True model.tetrisState } ! []
                _ -> if code > 48 && code < 58  -- Is a number
                    then
                        let (newState, cmd) = holdPiece (code - 49) model.tetrisState
                        in ( { model | tetrisState = newState }, cmd)
                    else model ! []

        Update time ->
            if model.paused || model.changingSettings
                then model ! []
                else case model.lastTime of
                    Nothing -> { model | lastTime = Just time } ! []
                    Just last ->
                        let delta = (time - last) / 1000
                            (newState, cmd) = updateTetris model.settings delta model.tetrisState
                        in
                            (
                                { model
                                | lastTime = Just time
                                , tetrisState = newState
                                }
                            , cmd
                            )

        SetSettings s -> { model | settings = s } ! []

        Started ->
            if model.settings.pieceSize > 0
            then { model | changingSettings = False } ! []
            else model ! []

view : Model -> Html Msg
view model =
    if model.changingSettings
        then viewSettings model
        else viewTetris model

viewSettings : Model -> Html Msg
viewSettings model =
    let set = model.settings
    in div [ id "gameS", align "center" ]
        [ text "Hard: "
        , input
            [ type_ "checkbox"
            , checked model.settings.hard
            , onClick (SetSettings { set | hard = not model.settings.hard } )
            ] []
        , br [] []
        , text "Number of triangles per piece: "
        , input
            [ type_ "number"
            , value <| if model.settings.pieceSize > 0
                then toString model.settings.pieceSize
                else ""
            , onInput
                (\st ->
                    if st == "" then SetSettings { set | pieceSize = 0 }
                    else case String.toInt st of
                        Ok x -> SetSettings { set | pieceSize = x }
                        _ -> SetSettings model.settings
                )
            ] []
        , br [] []
        , button [ onClick Started ] [ text "Start" ]
        ]

viewTetris : Model -> Html Msg
viewTetris model =
    div []
    [ div [ id "gameS", align "center" ]
        [
        S.svg
            [ id "blur"
            , Sa.height <| toString <| model.settings.scale * List.length model.tetrisState.blocks
            , Sa.width <| toString <| model.settings.scale * width model.tetrisState.blocks
            ]
            <| renderTetris model.settings Nothing model.tetrisState
        , S.svg
            [ id "game_next"
            , Sa.height <| toString <| model.settings.scale * List.length model.tetrisState.blocks
            , Sa.width <| toString <| model.settings.scale * width model.tetrisState.blocks
            ] <|
                let tetrisState = drop model.settings 0 False model.tetrisState
                in renderTetris model.settings (Just (255, 255, 255))
                    { tetrisState
                    | blocks = List.map ( List.map (always Empty)) tetrisState.blocks
                    }
        , S.svg
            [ id "game"
            , Sa.height <| toString <| model.settings.scale * List.length model.tetrisState.blocks
            , Sa.width <| toString <| model.settings.scale * width model.tetrisState.blocks
            ]
            <| renderTetris model.settings Nothing model.tetrisState
        ]
    , div [ id "nexts" ]
        <| List.map
            (\piece ->
            div [ class "next" ]
                [ S.svg
                    [ Sa.height <| toString <| model.settings.scale * List.length piece.shape
                    , Sa.width <| toString <| model.settings.scale * width piece.shape
                    ] <| renderGrid model.settings Nothing 0 0 piece.shape
                ]
            )
        <| List.take 3 model.tetrisState.nexts
    , div [ id "score" ]
        [ text <| "Score: " ++ toString model.tetrisState.score
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
                                [ Sa.height <| toString <| model.settings.scale * List.length hold.shape
                                , Sa.width <| toString <| model.settings.scale * width hold.shape
                                ] <| renderGrid model.settings Nothing 0 0 hold.shape
                            ]
            )
            model.tetrisState.hold
    , div [ id "pause" ]
        <| if model.paused
               then [ text "||" ]
               else [ text "" ]
    , div [ id "buttons" ]
        [ button [ onClick Restart ] [ text "Restart" ]
        ]
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
    | RGB (Int, Int, Int)

cols = [Red, Blue, Green, White, LBlue, Orange, Purple]

colStr : Maybe (Int, Int, Int) -> Color -> String
colStr blend c =
    let (r, g, b) =
            case c of
                Red ->    (255, 0,   0)
                Blue ->   (0,   0,   255)
                Green ->  (0,   255, 0)
                White ->  (255, 255, 255)
                Yellow -> (255, 255, 0)
                LBlue ->  (0,   255, 255)
                Orange -> (255, 120, 0)
                Purple -> (255, 0,   255)
                RGB x ->  x
        (r_, g_, b_) =
            case blend of
                Just (br, bg, bb) -> ((r + br) // 2, (g + bg) // 2, (b + bb) // 2)
                Nothing -> (r, g, b)
    in "rgb(" ++ toString r_ ++ "," ++ toString g_ ++ "," ++ toString b_ ++ ")"

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
    , score : Int
    , gameOver : Bool
    }

flatten : List (List a) -> List a
flatten x =
    case x of
        [] -> []
        (a::b) -> a ++ flatten b

hashPieceRaw : Blocks -> Int
hashPieceRaw blocks =
    let lst = flatten blocks
        h = List.foldr (\blk prev ->
                let cur =
                        case blk of
                            Empty -> 0
                            Filled DownLeft _ -> 1
                            Filled DownRight _ -> 2
                            Filled UpLeft _ -> 3
                            Filled UpRight _ -> 4
                            Filled Full _ -> 5
                            Split _ _ _ -> 0
                in (cur * hashPrimeMul + prev * 6) % hashPrimeMod
            ) 0 lst
    in h * 16 + List.length blocks

hashPiece : Blocks -> Int
hashPiece b =
    let d0 = { x = 0, y = 0, shape = b }
        h0 = hashPieceRaw d0.shape
        d1 = rotate d0
        h1 = hashPieceRaw d1.shape
        d2 = rotate d1
        h2 = hashPieceRaw d2.shape
        d3 = rotate d2
        h3 = hashPieceRaw d3.shape
    in Basics.min h0 <| Basics.min h1 <| Basics.min h2 h3

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
    List.repeat (Tuple.second dims) <|
    List.repeat (Tuple.first dims) Empty

holdPiece : Int -> TetrisState -> (TetrisState, Cmd Msg)
holdPiece idx state =
    case (getAt idx state.hold, state.dropping) of
        (Just piece, Just dropping) ->
            { state
            | hold =
                Debug.log "Hold" <|
                    setAt state.hold idx <| Just dropping
            , dropping = Nothing
            , score = floor <| toFloat state.score * 0.99
            } !
            case piece of
                Just piece ->
                    [ Random.generate SetDropping <| setPos state piece ]
                Nothing -> []
        a -> (always <| state ! []) <| Debug.log "Nope" a

updateTetris : Settings -> Float -> TetrisState -> (TetrisState, Cmd Msg)
updateTetris settings delta state =
    if state.gameOver
        then
            { state
            | dropping = Nothing
            , nexts = []
            } ! []
        else case state.dropping of
            Nothing ->
                case state.nexts of
                    (p::rest) ->
                        { state
                        | dropping = Just p
                        , nexts = rest
                        , gameOver =
                            Maybe.withDefault [] (List.head state.blocks)
                            |> List.any (\p -> isFilled p)
                        } !
                        if List.length rest < 3
                           then [ Random.generate SetDroppings <| nextsGenerator settings state ]
                           else []
                    [] ->
                        state ! [ Random.generate SetDroppings <| nextsGenerator settings state ]
            Just dropping ->
                let newDropping = { dropping | y = dropping.y + 1 }
                in
                    if fits state.blocks newDropping
                        then
                            { state
                            | dropping = Just newDropping
                            } ! []
                        else
                            let (newLines, amount) =
                                    removeWholeLines settings
                                    <| Maybe.withDefault state.blocks
                                    <| place state.blocks dropping
                            in
                                { state
                                | dropping = Nothing
                                , blocks = newLines
                                , score = state.score + 100 * 2 ^ amount * amount + 5
                                } ! []

shouldRemove : Settings -> List BlockState -> Bool
shouldRemove settings line =
    let nrTrisFilled =
            List.sum <|
                List.map
                    (\x ->
                    case x of
                        Filled Full _ -> 2
                        Filled _ _ -> 1
                        Split _ _ _ -> 2
                        _ -> 0
                ) line
    in if settings.hard
        then nrTrisFilled == 2 * List.length line
        else nrTrisFilled >= 2 * List.length line - 1

removeWholeLines : Settings -> Blocks -> (Blocks, Int)
removeWholeLines settings blocks =
    let removed =
            List.filter
                ( not << shouldRemove settings
                ) blocks
        removedAmount = List.length blocks - List.length removed
        newLines =
            List.repeat
                removedAmount
                ( List.repeat (width blocks) Empty )
    in (newLines ++ removed, removedAmount)

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

drop : Settings -> Int -> Bool -> TetrisState -> TetrisState
drop settings scoreDown clear state =
    case state.dropping of
        Just _ ->
            let (newState, _) = updateTetris settings (-1) state
            in case newState.dropping of
                Nothing -> if clear then newState else state
                Just _ ->
                    if scoreDown == 0
                       then drop settings dropScore clear { newState | score = newState.score + 1 }
                       else drop settings (scoreDown - 1) clear newState
        _ -> state

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


renderTetris : Settings -> Maybe (Int, Int, Int) -> TetrisState -> List (S.Svg msg)
renderTetris settings blend state =
    let placed =
            case state.dropping of
                Nothing -> state.blocks
                Just dropping -> Maybe.withDefault state.blocks <| place state.blocks dropping
    in renderGrid settings blend 0 0 placed

renderGrid settings blend oy ox =
    List.concat <<
        List.indexedMap (\y line ->
            List.concat <|
                List.indexedMap (\x blk ->
                    renderBlock settings blend (y + oy) (x + ox) blk
                )
                line
        )

renderBlock : Settings -> Maybe (Int, Int, Int) -> Int -> Int -> BlockState -> List (S.Svg msg)
renderBlock settings blend yp xp blk =
    let size = settings.scale
        y = yp * size
        x = xp * size
    in case blk of
        Filled Full col ->
                [ S.rect
                    [ Sa.x <| toString x
                    , Sa.y <| toString y
                    , Sa.width <| toString size
                    , Sa.height <| toString size
                    , Sa.style <| "fill:" ++ colStr blend col
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
                    , Sa.style <| "fill:" ++ colStr blend col
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
                    , Sa.style <| "fill:" ++ colStr blend col
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
                    , Sa.style <| "fill:" ++ colStr blend col
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
                    , Sa.style <| "fill:" ++ colStr blend col
                    ] []
                ]
        Split shape c1 c2 ->
            let first  = renderBlock settings blend yp xp ( Filled shape c1 )
                second = renderBlock settings blend yp xp ( Filled (rotateFill <| rotateFill shape) c2 )
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

isBad : Settings -> Dropping -> Bool
isBad settings piece =
    let w = width piece.shape
        h = List.length piece.shape
    in if w > Tuple.first dims || h > Tuple.second dims
        then True
        else if settings.hard
            then False
            else
                let triCount =
                        List.sum <| List.map
                            ( List.sum << List.map
                                (\p -> case p of
                                    Filled Full _ -> 0
                                    Empty -> 0
                                    Split _ _ _ -> 0
                                    _ -> 1
                                )
                            )
                        piece.shape
                in triCount > 2

-- Generate a piece with the area of the argument. One area unit is the same as half a square.
generatePieceWithArea : Settings -> Int -> Random.Generator Dropping
generatePieceWithArea settings area =
    Random.andThen randomRot
    <| Random.andThen
        (\piece ->
            if isBad settings piece
               then generatePieceWithArea settings area
               else rConst piece
        )
    <| case area of
        1 -> Re.constant ( Dropping 0 0 [ [ Filled DownRight Purple ] ] )
        _ ->
            let prev = generatePieceWithArea settings ( area - 1 )
                adder = Re.choices
                    [ Random.andThen addTri prev
                    , Random.andThen addTri prev
                    , Random.andThen addTri prev
                    , Random.andThen fillTri prev ]
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
                                    let newShape = setBlock yPlace x newPiece shape
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

nextsGenerator : Settings -> TetrisState -> Random.Generator (List Dropping)
nextsGenerator settings state =
    Random.andThen
        Rl.shuffle
        <| Re.combine
        <| List.map
            (always <| Random.andThen (setProps state) <|
                generatePieceWithArea settings settings.pieceSize)
            (List.range 0 5)

randCol : Blocks -> Random.Generator Blocks
randCol shape =
    let col = rConst <| hash2RGB <| hashPiece shape
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

fracpart : Float -> Float
fracpart x = x - toFloat (floor x)

rgb2f : (Int, Int, Int) -> (Float, Float, Float)
rgb2f (r, g, b) =
    ( toFloat r / 256
    , toFloat g / 256
    , toFloat b / 256)

gradient =
    [ rgb2f(169, 239, 103) -- Grönare grön
    , rgb2f(106, 226, 138) -- Lagom ljus (grön)
    , rgb2f(94, 229, 213) -- Ljusblå
    , rgb2f(96, 149, 234) -- Blå?
    , rgb2f(37, 27, 173) -- Indigo
    , rgb2f(106, 82, 211) -- Violett
    , rgb2f(151, 59, 191) -- Lila
    , rgb2f(232, 55, 140) -- Rosa
    , rgb2f(232, 79, 55) -- Orangeröd
    ]

blendidx : Int -> Float -> (Float, Float, Float)
blendidx idx lvl =
    let (r1, g1, b1) = Maybe.withDefault (0, 0, 0) <| getAt (idx % List.length gradient) gradient
        (r2, g2, b2) = Maybe.withDefault (0, 0, 0) <| getAt ((idx + 1) % List.length gradient) gradient
    in
        ( r1 * lvl + r2 * (1 - lvl)
        , g1 * lvl + g2 * (1 - lvl)
        , b1 * lvl + b2 * (1 - lvl)
        )

f2grad : Float -> (Float, Float, Float)
f2grad f =
    let f_ = f * toFloat (List.length gradient)
        idx = floor f_
        blend = fracpart f_
    in blendidx idx blend

hash2RGB : Int -> Color
hash2RGB hash =
    let hue = toFloat hash / hashPrimeMod
        -- sat = (mod3 (toFloat hash) + 1) / 3
        (r, g, b) = f2grad hue
        -- (r_, g_, b_) = (r * sat + (1 - sat), g * sat + (1 - sat), b * sat + (1 - sat))
    in RGB (floor (255 * r), floor (255 * g), floor (255 * b))
