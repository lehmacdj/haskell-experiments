module WriterFunctions where

import Control.Monad.Writer

newtype PartitionedWrite r c = PartitionedWrite
    { unPartitionedWrite :: r -> c }

instance (Monoid c) => Monoid (PartitionedWrite r c) where
    mempty = const mempty
    f <> g = \x -> f x <> g x
