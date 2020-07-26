{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module LensDeriving where

import Control.Lens

data Foo a = Foo { _fooX :: Int, _fooY :: a }
newtype Bar = Bar { _barX :: Char }
makeFields ''Foo
makeFields ''Bar
