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

```haskell
gridDim = 20
startGrid :: Univ GameOfLife.Cell
startGrid = makeUniv gridDim (\y x -> GameOfLife.bool2cell ((y ^ 13 `mod` 1023 <= 512) && (x ^ 17 `mod` 2047 <= 512)))

main = mainWith $ mkCAGif GameOfLife.gameOfLifeCA startGrid 20
```

Brians Brain
------------

```haskell

gridDim = 25
startGrid :: Univ BriansBrain.Cell
startGrid = makeUniv gridDim (\y x -> if (y ^ 13 `mod` 1023 <= 5)
                                      then
                                        if (x ^ 17 `mod` 2047 <= 5)
                                            then BriansBrain.On
                                            else BriansBrain.Dying
                                      else
                                        BriansBrain.Off)

main = mainWith $ mkCAGif BriansBrain.briansBrainCA startGrid 100
```


