module Main exposing (..)

import Dict
import Playground exposing (..)
import Set
import String


type CellState
    = Alive
    | Dead


type alias Grid =
    Dict.Dict ( Int, Int ) CellState


type alias GameState =
    { cellSize : Number
    , grid : Grid
    , isRunning : Bool
    , frame : Int
    , spacePressed : Bool
    , numPressed : Bool
    }


main =
    game view
        update
        { cellSize = 20
        , grid = Dict.empty
        , isRunning = False
        , frame = 0
        , spacePressed = False
        , numPressed = False
        }


gliderGun =
    [ ( 6, 1 )
    , ( 6, 2 )
    , ( 7, 1 )
    , ( 7, 2 )
    , ( 6, 11 )
    , ( 7, 11 )
    , ( 8, 11 )
    , ( 5, 12 )
    , ( 9, 12 )
    , ( 4, 13 )
    , ( 4, 14 )
    , ( 10, 13 )
    , ( 10, 14 )
    , ( 7, 15 )
    , ( 5, 16 )
    , ( 9, 16 )
    , ( 7, 17 )
    , ( 6, 17 )
    , ( 8, 17 )
    , ( 7, 18 )
    , ( 6, 21 )
    , ( 5, 21 )
    , ( 4, 21 )
    , ( 6, 22 )
    , ( 5, 22 )
    , ( 4, 22 )
    , ( 7, 23 )
    , ( 3, 23 )
    , ( 7, 25 )
    , ( 3, 25 )
    , ( 8, 25 )
    , ( 2, 25 )
    , ( 4, 35 )
    , ( 4, 36 )
    , ( 5, 35 )
    , ( 5, 36 )
    ]


xkcdMemorial =
    [ ( 1, 3 )
    , ( 1, 4 )
    , ( 1, 5 )
    , ( 2, 3 )
    , ( 2, 5 )
    , ( 3, 3 )
    , ( 3, 5 )
    , ( 4, 4 )
    , ( 5, 4 )
    , ( 6, 4 )
    , ( 7, 4 )
    , ( 8, 3 )
    , ( 9, 3 )
    , ( 8, 5 )
    , ( 9, 5 )
    , ( 5, 5 )
    , ( 5, 3 )
    , ( 6, 2 )
    , ( 5, 1 )
    , ( 6, 6 )
    , ( 7, 7 )
    ]



-- VIEW


view : Computer -> GameState -> List Shape
view computer state =
    Dict.keys state.grid
        |> List.map (\p -> cell computer.screen state (Tuple.first p) (Tuple.second p))


cell : Screen -> GameState -> Int -> Int -> Shape
cell screen { cellSize, grid } row col =
    grid
        |> Dict.get ( row, col )
        |> Maybe.withDefault Dead
        |> cellColor
        |> squareFromCorner (cellPadding cellSize)
        |> moveTopLeft screen
        |> moveFromCorner (cellSize * toFloat col) (cellSize * toFloat row)


cellPadding : Number -> Number
cellPadding cellSize =
    cellSize * 0.9


cellColor : CellState -> Color
cellColor state =
    case state of
        Dead ->
            lightGray

        Alive ->
            black


squareFromCorner : Number -> Color -> Shape
squareFromCorner size color =
    let
        offset =
            size / 2
    in
    square color size
        |> move offset -offset


moveFromCorner : Number -> Number -> Shape -> Shape
moveFromCorner x y shape =
    move x -y shape


moveTopLeft : Screen -> Shape -> Shape
moveTopLeft { top, left } shape =
    move left top shape



-- UPDATE


update { mouse, keyboard, screen } state =
    { state | frame = state.frame + 1 }
        |> initGrid screen
        |> loadPattern keyboard screen
        |> handleClick mouse screen
        |> handleSpacebar keyboard
        |> handleReset keyboard
        |> runGame


initGrid : Screen -> GameState -> GameState
initGrid screen state =
    if Dict.isEmpty state.grid then
        { state | grid = newGrid screen state.cellSize }

    else
        state


loadPattern : Keyboard -> Screen -> GameState -> GameState
loadPattern keyboard screen state =
    if state.numPressed && Set.member "1" keyboard.keys then
        { state | grid = gridFromPattern (moveToCenter xkcdMemorial state.grid) state.grid, isRunning = False, numPressed = isNumPressed keyboard }

    else if state.numPressed && Set.member "2" keyboard.keys then
        { state | grid = gridFromPattern gliderGun state.grid, isRunning = False, numPressed = isNumPressed keyboard }

    else
        { state | numPressed = isNumPressed keyboard }


isNumPressed : Keyboard -> Bool
isNumPressed { keys } =
    List.range 0 9
        |> List.map String.fromInt
        |> Set.fromList
        |> Set.intersect keys
        |> Set.isEmpty


moveToCenter : List ( Int, Int ) -> Grid -> List ( Int, Int )
moveToCenter pattern grid =
    pattern
        |> List.map (\x -> ( Tuple.first x + Tuple.first (centerCell grid), Tuple.second x + Tuple.second (centerCell grid) ))


centerCell : Grid -> ( Int, Int )
centerCell grid =
    grid
        |> Dict.keys
        |> List.unzip
        |> Tuple.mapBoth List.maximum List.maximum
        |> Tuple.mapBoth (Maybe.withDefault 0) (Maybe.withDefault 0)
        |> Tuple.mapBoth (\x -> x // 2) (\x -> x // 2)


gridFromPattern : List ( Int, Int ) -> Grid -> Grid
gridFromPattern pattern grid =
    List.foldl toggleCell grid pattern


handleClick : Mouse -> Screen -> GameState -> GameState
handleClick mouse screen state =
    if mouse.click then
        { state | grid = toggleCell (convertToGrid screen state.cellSize mouse) state.grid }

    else
        state


handleSpacebar : Keyboard -> GameState -> GameState
handleSpacebar { space } state =
    if state.spacePressed && not space then
        { state | isRunning = not state.isRunning, spacePressed = space }

    else
        { state | spacePressed = space }


handleReset : Keyboard -> GameState -> GameState
handleReset { keys } state =
    if Set.member "r" keys then
        { state | grid = Dict.map (\k -> \a -> Dead) state.grid, isRunning = False }

    else
        state


runGame : GameState -> GameState
runGame state =
    if state.isRunning && modBy 5 state.frame == 0 then
        { state | grid = nextState state.grid }

    else
        state


nextState : Grid -> Grid
nextState grid =
    Dict.toList grid
        |> List.map (\t -> ( Tuple.first t, nextCellState grid (Tuple.first t) ))
        |> Dict.fromList


nextCellState : Grid -> ( Int, Int ) -> CellState
nextCellState grid key =
    case ( cellNeighbours grid key, getCellState grid key ) of
        ( ( 2, _ ), Alive ) ->
            Alive

        ( ( 3, _ ), _ ) ->
            Alive

        ( ( _, _ ), _ ) ->
            Dead


cellNeighbours : Grid -> ( Int, Int ) -> ( Int, Int )
cellNeighbours grid ( row, col ) =
    [ ( row + 1, col - 1 )
    , ( row + 1, col )
    , ( row + 1, col + 1 )
    , ( row, col + 1 )
    , ( row - 1, col + 1 )
    , ( row - 1, col )
    , ( row - 1, col - 1 )
    , ( row, col - 1 )
    ]
        |> List.map (getCellState grid)
        |> List.partition (\s -> s == Alive)
        |> Tuple.mapBoth List.length List.length


getCellState : Grid -> ( Int, Int ) -> CellState
getCellState grid key =
    Maybe.withDefault Dead (Dict.get key grid)


toggleCell : ( Int, Int ) -> Grid -> Grid
toggleCell index grid =
    Dict.update index toggleCellState grid


convertToGrid : Screen -> Number -> Mouse -> ( Int, Int )
convertToGrid screen cellSize { x, y } =
    ( floor ((screen.top - y) / cellSize)
    , floor ((x - screen.left) / cellSize)
    )


toggleCellState : Maybe CellState -> Maybe CellState
toggleCellState state =
    case state of
        Just Dead ->
            Just Alive

        Just Alive ->
            Just Dead

        Nothing ->
            Nothing


newGrid : Screen -> Number -> Grid
newGrid screen cellSize =
    let
        calculatedSize =
            gridSize screen cellSize

        rowSize =
            Tuple.first calculatedSize - 1

        colSize =
            Tuple.second calculatedSize - 1
    in
    createPositions rowSize colSize colSize []
        |> List.map (\c -> ( c, Dead ))
        |> Dict.fromList


createPositions : Int -> Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
createPositions rowSize colSize orgColSize acc =
    case ( rowSize, colSize ) of
        ( 0, 0 ) ->
            ( rowSize, colSize ) :: acc

        ( _, 0 ) ->
            createPositions (rowSize - 1) orgColSize orgColSize (( rowSize, colSize ) :: acc)

        _ ->
            createPositions rowSize (colSize - 1) orgColSize (( rowSize, colSize ) :: acc)


gridSize :
    Screen
    -> Number
    -> ( Int, Int ) -- rows, `cols
gridSize { width, height } cellSize =
    ( floor (height / cellSize), floor (width / cellSize) )
