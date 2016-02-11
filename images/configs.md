Seeds
-----

```haskell

gridDim = 50
startGrid :: Univ Seeds.Cell
startGrid = makeUniv gridDim (\y x -> Seeds.bool2cell (y `mod` 5 < 3 && x `mod` 2 == 0 && (x + y) `mod` 3 < 1))

main = mainWith $ mkCAGif Seeds.seedsCA startGrid 500

```

Life
----
