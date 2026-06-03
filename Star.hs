{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module Star where
{- HLINT ignore "Use infix" -}
{- HLINT ignore "Replace case with fromMaybe" -}
import Data.Array
import Data.List


data Shape =
    Sized Integer
  | Product [(String, Shape)]
  | Concat  [(String, Shape)]
  deriving (Eq)


instance Show Shape where
  show (Sized n) = "#" ++ show n
  show (Product fields) =
    "{" ++ intercalate ", " [label ++ " : " ++ show ix | (label, ix) <- fields] ++ "}"
  show (Concat branches) =
    "<" ++ intercalate ", " [label ++ " : " ++ show ix | (label, ix) <- branches] ++ ">"

data Index =
      OneD Integer
    | ProductIx [(String, Index)]
    | ConcatIx (String, Index)
  deriving (Eq, Ord)

instance Show Index where
  show (OneD n) = show n
  show (ProductIx fields) =
    "{" ++ intercalate ", " [label ++ " = " ++ show ix | (label, ix) <- fields] ++ "}"
  show (ConcatIx (str, ix)) = "<" ++ str ++ " = " ++ show ix ++ ">"


--Checks validity of shape
checkShape :: Shape -> Bool
checkShape s =
  case s of
    (Sized n) -> n>0
    --Means empty Products are not correct
    (Product []) -> False
    (Product fields) -> checkFieldsAndBranches fields
    --Means empty Concats are not correct
    (Concat []) -> False
    (Concat branches) -> checkFieldsAndBranches branches

--For Product and Concat Shapes
checkFieldsAndBranches :: [(String, Shape)] -> Bool
checkFieldsAndBranches [] = True
checkFieldsAndBranches ((_, subShape):rest) =
  checkShape subShape && checkFieldsAndBranches rest


allIndices :: Shape -> [Index]
allIndices (Sized n) = [OneD x | x <- [0..n-1]]

allIndices (Concat []) = []
-- Concat case
allIndices (Concat ((tag, subShape): branches)) =
  [ConcatIx (tag, subIndex) | subIndex <- allIndices subShape] ++ allIndices (Concat branches)

allIndices (Product []) = [ProductIx []]
-- Product case
allIndices (Product ((label, subShape): rest)) =
  [ ProductIx ((label, subIndex) : restIndices)
  | subIndex <- allIndices subShape
  , ProductIx restIndices <- allIndices (Product rest)
  ]


-- Finds shape with specific label in Product shape
projectShape :: Shape -> String -> Maybe Shape
projectShape (Sized _) _ = Nothing
projectShape (Concat _) _ = Nothing
projectShape (Product []) _ = Nothing
projectShape (Product ((label, subShape):rest)) str =
  if label == str then Just subShape
  else projectShape (Product rest) str

--Find labelled coordinate in index
proj :: Index -> String -> Maybe Integer
proj (OneD _) _ = Nothing
proj (ConcatIx _) _ = Nothing
proj (ProductIx []) _= Nothing
proj (ProductIx ((label, OneD n):rest)) str =
  if label == str then Just n
  else proj (ProductIx rest) str
proj (ProductIx (_:rest)) str =  proj (ProductIx rest) str

--find lower and upper bounds of different types of shape
lowerAndUpper :: Shape -> (Index, Index)
lowerAndUpper (Sized n) = (OneD 0, OneD (n-1))
lowerAndUpper (Product fields) =
  (ProductIx [(label, lower) | (label, subShape) <- fields, let (lower, _) = lowerAndUpper subShape],
   ProductIx [(label, upper) | (label, subShape) <- fields, let (_, upper) = lowerAndUpper subShape])

-- Concat case
lowerAndUpper (Concat branches) =
  let
    (firstLabel, firstShape) = head branches
    (lastLabel, lastShape) = last branches
    (firstLower, _) = lowerAndUpper firstShape
    (_, lastUpper) = lowerAndUpper lastShape
  in (ConcatIx (firstLabel, firstLower), ConcatIx (lastLabel, lastUpper))



instance Ix Index where
  -- Range
  range :: (Index, Index) -> [Index]
  range (OneD lowerBound, OneD upperBound) = [OneD n | n <- [lowerBound .. upperBound]]

  range (ProductIx [], ProductIx []) = [ProductIx []]
  -- Product case
  range (ProductIx ((lowerLabel, lowerIndex):lowerFields), ProductIx ((upperLabel, upperIndex):upperFields)) =
    if lowerLabel /= upperLabel then []
    else
      [ ProductIx ((lowerLabel, fieldIndex) : remainingFields)
      | fieldIndex <- range (lowerIndex, upperIndex)
      , ProductIx remainingFields <- range (ProductIx lowerFields, ProductIx upperFields)
      ]

  -- Concat case
  range (ConcatIx (lowerTag, lowerIndex), ConcatIx (upperTag, upperIndex)) =
    if lowerTag /= upperTag then []
    else
      let valueRange = range (lowerIndex, upperIndex)
      in
        [ConcatIx (lowerTag, innerIndex) | innerIndex <- valueRange]


  -- Bounds check
  inRange :: (Index, Index) -> Index -> Bool
  inRange arrBounds targetIndex = targetIndex `elem` range arrBounds
 
  index :: (Index, Index) -> Index -> Int
  index (OneD lowerBound, OneD _) (OneD n) =
    fromIntegral (n - lowerBound)

  index (ProductIx lowerFields, ProductIx upperFields) (ProductIx targetFields) =
    case elemIndex (ProductIx targetFields)
                  (range (ProductIx lowerFields, ProductIx upperFields)) of
      Just offset -> offset
      Nothing -> error "value is not in range"

  index (ConcatIx lowerBound, ConcatIx upperBound) (ConcatIx targetIndex) =
    case elemIndex (ConcatIx targetIndex)
                  (range (ConcatIx lowerBound, ConcatIx upperBound)) of
      Just offset -> offset
      Nothing -> error "value is not in range"




-- Star arr data structure
data StarArray a =
  MkStarArray Shape (Array Index a)
  deriving (Eq, Show)


-- Make star array from shape
mkArray :: Shape -> (Index -> a) -> StarArray a
mkArray shape f =
  let
    arrBounds = lowerAndUpper shape
    entries = [(ix, f ix) | ix <- allIndices shape]
    arr = array arrBounds entries
  in MkStarArray shape arr

-- finds value stored in starArray at given index
valAtIndex :: StarArray a -> Index -> Maybe a
valAtIndex (MkStarArray shape arr) ix =
  if inRange (lowerAndUpper shape) ix
    then Just (arr ! ix)
    else Nothing

--HELP
shapeBroadcast :: Shape -> Shape -> Maybe Shape
shapeBroadcast (Sized x) (Sized y)
  -- Same sized case
  | x == y = Just (Sized x)
  -- x is 1
  | x == 1 = Just (Sized y)
  -- y is 1
  | y == 1 = Just (Sized x)
  | otherwise = Nothing

shapeBroadcast (Product []) (Product []) = Just (Product [])
shapeBroadcast (Product ((field1, n1):xs1)) (Product ((field2, n2):xs2))
  | field1 == field2 =
      case (shapeBroadcast n1 n2, shapeBroadcast (Product xs1) (Product xs2)) of
        (Just s1, Just (Product restFields)) ->
          Just (Product ((field1, s1) : restFields))
        _ ->
          Nothing
  | otherwise = Nothing



