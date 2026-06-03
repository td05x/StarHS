{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module StarPi where

-- Datakind for shapes
data ShapeKind = 
    SizedK
  | ProductK
  | ConcatK

-- Main Shape type
data Shape (s :: ShapeKind) where
  Sized :: Int -> Shape 'SizedK
  Product :: [(String, Shape s)] -> Shape 'ProductK -- s has to be all same type ugh
  Concat :: [(String, Shape s)] -> Shape 'ConcatK -- same issue
-- Main index type
data Index (s :: ShapeKind) where 
    OneD :: Int -> Index 'SizedK
    ProductIx :: [(String, Index s)] -> Index 'ProductK -- same issue
    ConcatIx :: (String, Index s) -> Index 'ConcatK -- same issue


