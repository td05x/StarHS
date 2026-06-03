{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{- HLINT ignore "Redundant ==" -}
{- HLINT ignore "Redundant if" -}
{- HLINT ignore "Replace case with fromMaybe" -}
module GameOfLife where
import Star

--sets number of columns in grid
numCols :: Integer
numCols = 10

--sets number of rows in grid
numRows :: Integer
numRows = 10

--specifies the shape for grid arr generation
gridShape :: Shape
gridShape = Product [("row", Sized numRows), ("col", Sized numCols)]

--empty grid seed used initially just to test
emptyGrid :: StarArray Bool
emptyGrid = mkArray gridShape
            (const False)

--Example seeds
blockSeed :: StarArray Bool
blockSeed = mkArray gridShape
            (\ix -> ix `elem`
              [ ProductIx [("row", OneD 3), ("col", OneD 3)]
              , ProductIx [("row", OneD 3), ("col", OneD 4)]
              , ProductIx [("row", OneD 4), ("col", OneD 3)]
              , ProductIx [("row", OneD 4), ("col", OneD 4)]
              ])

blinkerSeed :: StarArray Bool
blinkerSeed = mkArray gridShape
              (\ix -> ix `elem`
                [ ProductIx [("row", OneD 4), ("col", OneD 3)]
                , ProductIx [("row", OneD 4), ("col", OneD 4)]
                , ProductIx [("row", OneD 4), ("col", OneD 5)]
                ])

gliderSeed :: StarArray Bool
gliderSeed = mkArray gridShape
             (\ix -> ix `elem`
               [ ProductIx [("row", OneD 1), ("col", OneD 2)]
               , ProductIx [("row", OneD 2), ("col", OneD 3)]
               , ProductIx [("row", OneD 3), ("col", OneD 1)]
               , ProductIx [("row", OneD 3), ("col", OneD 2)]
               , ProductIx [("row", OneD 3), ("col", OneD 3)]
               ])

--Displays grid using helper functions
showGrid :: StarArray Bool -> String
showGrid starArr =
  unlines [showRow starArr r | r <- [0 .. numRows - 1]]

--Shows row using showCell
showRow :: StarArray Bool -> Integer -> String
showRow starArr r =
  [ showCell (cellAt starArr (ProductIx [("row", OneD r), ("col", OneD c)]))
  | c <- [0 .. numCols - 1]
  ]

--If cell is Alive (True) displays # otherwise .
showCell :: Bool -> Char
showCell True = '#'
showCell False = '.'


--Returns whether a cell at an index is Alive or Dead
cellAt :: StarArray Bool -> Index -> Bool
cellAt starArray ix =
  case valAtIndex starArray ix of
    Nothing -> False
    Just x -> x


--Returns list of indexes of neigboring cells
neighborCoords :: Index -> [Index]
neighborCoords (ProductIx [("row", OneD x), ("col", OneD y)]) =
  let
    offsets = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]
  in
   [ProductIx [("row", OneD (x + dx)), ("col", OneD (y + dy))] | (dx, dy) <- offsets]

--Returns count for amount of alive neighbors (needed for conway rules)
liveNeighborCount :: StarArray Bool -> Index -> Int
liveNeighborCount starArr ix =
  let
    neighborList = neighborCoords ix
    numOfLiveNeighbors =
      length (filter id [cellAt starArr val | val <- neighborList])
  in
    numOfLiveNeighbors

--Works out what cells next state will be based on num of alive neighbors and whether it is alive or not
nextCellState :: StarArray Bool -> Index -> Bool
nextCellState starArr ix =
  let
    liveNeighbors = liveNeighborCount starArr ix
    currentCellState = cellAt starArr ix
  in
    if
      currentCellState == True && liveNeighbors < 2 then False
    else if
      currentCellState == True && (liveNeighbors == 2 || liveNeighbors == 3) then True
    else if
      currentCellState == True && liveNeighbors > 3 then False
    else if
      currentCellState == False && liveNeighbors == 3 then True
    else
      False

--Updates grid using nextCellState on each cell
updateGrid :: StarArray Bool -> StarArray Bool
updateGrid starArr =
  mkArray gridShape (nextCellState starArr)

--Prints grid in terminal
printGrid :: StarArray Bool -> IO ()
printGrid starArr =
  putStrLn (showGrid starArr)

--Uses printGrid to print each step up to specified number of steps
runSteps :: Int -> StarArray Bool -> IO ()
runSteps n starArr
  | n <= 0 = pure ()
  | otherwise = do
      printGrid starArr
      runSteps (n - 1) (updateGrid starArr)
