module Set where

import Data.Function

data Attribute = Shape | Texture | Color | Count
    deriving (Show, Eq, Ord)

data Variant = A | B | C
    deriving (Show)

data Card = Card
    { shape :: Variant
    , texture :: Variant
    , color :: Variant
    , count :: Variant
    } deriving (Show)

type Board = [Card]

data AttrProxy = AttrProxy
    { attribute :: Attribute
    , cardCount :: Int
    } deriving (Show)

instance Eq AttrProxy where
    (==) = (==) `on` cardCount

instance Ord AttrProxy where
    compare = compare `on` cardCount

newtype Index = Index
    { attrList :: [AttrProxy]
    } deriving (Show)
