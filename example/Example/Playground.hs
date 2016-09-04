{-# OPTIONS_GHC -w -fwarn-incomplete-record-updates -fwarn-missing-fields #-}
module Example.Playground where

-- data Foo = Foo { x :: Int, y :: Int }
--          | Bar

-- f :: Foo -> Foo
-- f foo = foo { x = 6 }

-- foo = Foo { x = 6 }

data Foo = Bar {unBar :: Int} | Baz {unBaz :: Int}

main = print $ unBaz (Bar {unBar = 0})
