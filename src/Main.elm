module Main exposing (..)
import Dict
import Playground exposing (..)

type CellState = Alive | Dead
type alias Grid = Dict.Dict (Int, Int) CellState

type alias GameState = {
  cellSize: Number,
  cellPadding: Number,
  grid: Grid,
  isRunning: Bool,
  frame: Int,
  lastSpace: Bool
  }

main = game view update {
  cellSize = 30,
  cellPadding = 2 ,
  grid = Dict.empty,
  isRunning = False,
  frame = 0,
  lastSpace = False
  }

-- VIEW
view: Computer -> GameState -> List Shape
view computer state =
  Dict.keys state.grid
  |> List.map (\p -> cell computer.screen state (Tuple.first p) (Tuple.second p))


cell: Screen -> GameState -> Int -> Int -> Shape
cell screen {cellSize, cellPadding, grid} row col =
  grid
  |> Dict.get (row, col)
  |> Maybe.withDefault Dead
  |> cellColor
  |> squareFromCorner (cellSize - cellPadding)
  |> moveTopLeft screen
  |> moveFromCorner (cellSize * (toFloat col)) (cellSize * (toFloat row))

cellColor: CellState -> Color
cellColor state =
  case state of
    Dead -> lightGray
    Alive -> black

squareFromCorner: Number -> Color -> Shape
squareFromCorner size color =
    let
      offset = size / 2
    in
    square color size
    |> move offset -offset


moveFromCorner: Number -> Number -> Shape -> Shape
moveFromCorner x y shape =
  move x -y shape

moveTopLeft: Screen -> Shape -> Shape
moveTopLeft {top, left} shape =
  move left top shape


-- UPDATE
update {mouse, keyboard, screen} state =
  {state | frame = state.frame + 1}
  |> initGrid screen
  |> handleClick mouse screen
  |> handleSpacebar keyboard
  |> runGame


initGrid: Screen -> GameState -> GameState
initGrid screen state =
  if Dict.isEmpty state.grid then
    {state | grid = newGrid screen state.cellSize}
  else
    state


handleClick: Mouse -> Screen -> GameState -> GameState
handleClick mouse screen state =
  if mouse.click then
    {state | grid = toggleCell state.grid (convertToGrid screen state.cellSize mouse)}
  else
    state


handleSpacebar: Keyboard -> GameState -> GameState
handleSpacebar {space} state =
  if state.lastSpace && not space then
    {state | isRunning = not state.isRunning, lastSpace = space}
  else
    {state | lastSpace = space}


runGame: GameState -> GameState
runGame state =
  if state.isRunning && modBy 5 state.frame == 0 then
    {state | grid = nextState state.grid}
  else
    state


nextState: Grid -> Grid
nextState grid =
  Dict.toList grid
  |> List.map (\t -> (Tuple.first t, nextCellState grid (Tuple.first t)))
  |> Dict.fromList


nextCellState: Grid -> (Int, Int) -> CellState
nextCellState grid key =
  case (cellNeighbours grid key, getCellState grid key) of
    ((2, _), Alive) -> Alive
    ((3, _), _) -> Alive
    ((_, _), _) -> Dead


cellNeighbours: Grid -> (Int, Int) -> (Int, Int)
cellNeighbours grid (row, col) =
  [
    (row + 1, col - 1),
    (row + 1, col),
    (row + 1, col + 1),
    (row, col + 1),
    (row - 1, col + 1),
    (row - 1, col),
    (row - 1, col - 1),
    (row, col - 1)
  ]
  |> List.map (getCellState grid)
  |> List.partition (\s -> (s == Alive))
  |> Tuple.mapBoth List.length List.length


getCellState: Grid -> (Int, Int) -> CellState
getCellState grid key = Maybe.withDefault Dead (Dict.get key grid)


toggleCell: Grid -> (Int, Int) -> Grid
toggleCell grid index = Dict.update index toggleCellState grid


convertToGrid: Screen -> Number -> Mouse -> (Int, Int)
convertToGrid screen cellSize {x, y} =
  (
    floor ((screen.top - y) / cellSize),
    floor ((x - screen.left) / cellSize)
   )


toggleCellState: Maybe CellState -> Maybe CellState
toggleCellState state =
  case state of
    Just Dead -> Just Alive
    Just Alive -> Just Dead
    Nothing -> Nothing


newGrid: Screen -> Number -> Grid
newGrid screen cellSize =
  let
    calculatedSize = gridSize screen cellSize
    rowSize = Tuple.first calculatedSize - 1
    colSize = Tuple.second calculatedSize - 1
  in
  createPositions rowSize colSize colSize []
  |> List.map (\c -> (c, Dead))
  |> Dict.fromList


createPositions: Int -> Int -> Int -> List (Int, Int) -> List (Int, Int)
createPositions rowSize colSize orgColSize acc =
    case (rowSize, colSize) of
      (0, 0) -> (rowSize, colSize) :: acc
      (_, 0) -> createPositions (rowSize - 1) orgColSize orgColSize ((rowSize, colSize) :: acc)
      _ -> createPositions rowSize (colSize - 1) orgColSize ((rowSize, colSize) :: acc)


gridSize: Screen -> Number -> (Int, Int) -- rows, `cols
gridSize {width, height} cellSize = (floor (height / cellSize), floor (width / cellSize))
