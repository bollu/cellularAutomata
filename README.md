## Cellular Automata


Pretty cellular automata in Haskell. The aim is to have most Cellular Automata implemented in this
package so it can serve as a reference / library to write Cellular Automata.


### 1. Game of Life

![game-of-life-gif](https://raw.githubusercontent.com/bollu/cellularAutomata/master/images/game-of-life.gif)

##### Ruleset
```haskell
stepCell :: Grid -> Cell
stepCell grid = 
    cell'
    where
        cell' = if numNeighbours > 3 then Off
                else if numNeighbours < 2 then Off
                else if cell == Off && numNeighbours == 3 then On
                else cell
        cell = extract grid 
        numNeighbours = liveNeighbourCount $ grid

```

### 2. Seeds
![seeds-gif](https://github.com/bollu/cellularAutomata/blob/master/images/seeds.gif)

##### Ruleset

```haskell
stepCell :: Grid -> Cell
stepCell grid = 
    cell'
    where
        cell' = if numNeighbours == 2 then On
                else Off
        numNeighbours = liveNeighbourCount $ grid

```
### 3. Brian's brain

![brians-brain-gif](https://github.com/bollu/cellularAutomata/blob/master/images/brians-brain.gif)


##### Ruleset
```haskell
stepCell :: Grid -> Cell
stepCell grid = 
    cell'
    where
        cell' = if cell == Off && numNeighbours == 2 then On
                else if cell == On then Dying
                else Off
        cell = extract grid 
        numNeighbours = liveNeighbourCount $ grid
```

### 3. 1D cyclic Cellular Automata

![1d-cyclic.gif](https://github.com/bollu/cellularAutomata/blob/master/images/cyclic1d.gif)

##### Ruleset

```haskell
stepCell :: Simulation -> Cell
stepCell s =
    cell'
    where
        cell = extract s 
        cell' = if hasNextNeighbour (getRingZipperNeighbours s)
           then Cell { value = (Cyclic1D.value cell + 1) `mod` (total cell), total = total cell}
           else cell
        hasNextNeighbour neighbours = any (\c -> Cyclic1D.value c == ((Cyclic1D.value cell) + 1) `mod` (total cell)) neighbours
```

### 4. 2D cyclic Cellular Automata

![2d-cyclic.gif](https://github.com/bollu/cellularAutomata/blob/master/images/cyclic2d.gif)

##### Ruleset

```haskell
stepCell :: Grid -> Cell
stepCell s =
    cell'
    where
        cell = extract s 
        cell' = if hasNextNeighbour (getUnivNeighbours s)
           then Cell { val = (val cell + 1) `mod` (total cell), total = total cell}
           else cell
        hasNextNeighbour neighbours = any (\c -> val c == ((val cell) + 1) `mod` (total cell)) neighbours
```


### Design

This is an exploration of the Haskell design space to create Cellular Automata.

I finally settled on using [`Comonads`](http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html) to represent the grid space of the cellular automata.
The difference between this implementation and many others in the wild is that this one
has a *finite* grid, which makes writing the instances for `Zipper` and `Comonad` harder. 

This will be refactored into a library that allows one to create cellular automata by simply
specifying the ruleset and the way to draw a *single* cell. The library will extrapolate the data
to allow rendering the entire grid.

As of now, since it uses [diagrams](http://projects.haskell.org/diagrams/) with the `Cairo` backend to render, it is somewhat slow to
create GIFs. I hope to improve this by writing a separate `OpenGL` backend sometime.

### Contributing

Please send a PR to create more Cellular Automata by using an existing cellular automata file
(for example, `Seeds`(https://github.com/bollu/cellularAutomata/blob/master/src/Seeds.hs)).


If someone knows how to make GIF rendering faster, that would be of great help as well!


As a college student, I write code for passion projects like this on my free time. 
If you want to support me to see more stuff like this, please

[![Support via Gratipay](https://cdn.rawgit.com/gratipay/gratipay-badge/2.3.0/dist/gratipay.svg)](https://gratipay.com/bollu/)


### Theory

#### Motivating Comonads

As stated before, this simulation uses the `Comonad` typeclass to model cellular automata. There are multiple ways of looking at this algebra,
and one way to think of them is a structure that can automatically convert "global-to-local" transforms into "global-to-global" transforms.


For example, in a cellular automata, the "global-to-local" transformation is updating the state of _one_ Cell by reading the cell's neighbours.
The neighbour state is the global state, which is used to update the local state of the cell. This can be thought of as the type
```haskell
Grid -> Cell
```


where `Grid` is the grid in which the cellular automata is running, and `Cell` is the new state of the cell. However, the question that immediately arises
is - which cell? the answer is that, the `Grid` not only encodes the state of the cellular automata, but also a __focused cell__ which is updated.


The `Grid` is not just a grid, it is a grid with a cell that it is targeted on. However, this seems ridiculous, since we have simply added
extra complexity (that of focusing on a particular cell) with zero gains in benefit. 



The nice part of a `Comonad` is that if we have a structure that knows how to do a "focused update", the `Comonad` enables us to extend this
to the entire structure.
Written in types, it is along the lines of
```haskell
Grid -> (Grid -> Cell) -> Grid
```

If we think of grid as a container of cells (or as a functor `w`), this gives us the new type
```haskell
Grid -> (Grid -> Cell) -> Grid
-- replace Grid with w Cell
w Cell -> (w Cell -> Cell) -> w Cell
-- replace Cell with type variable a
w a -> (w a -> a) -> w a
-- generalize type even further, by allowing the
-- output type to differ
-- (this is shown to be possible with an implementation later on)
w a -> (w a -> b) -> w b
```
Note that this rewrite exploited the fact that a `Grid` is simply a functor (collection) of `Cell`s, and then used this to 
rewrite the type signature.


The type signature
```haskell
w a -> (w a -> b) -> w b
```
can be sharply contrasted with the monadic `>>= (bind)` as 
```haskell
>>= :: m a -> (a -> m b) -> m b
```

Indeed, these structures are dual, which is why there are called as `Comonad`, which is also why I
picked `w` as the symbol for `Comonad` (which is an upside-down `m` for `Monad`). It is usually called as "cobind", and is
written as
```haskell
=>> :: w a -> (w a -> b) -> w b
```
with the interpretation that it takes a global structure `w a` which is focused on some `a` in the `w a`, and then
takes a transform that updates the focused `a` in the `w a ` to a `b`. Given these two pieces of information, the
Comonad automatically updates every single `a`, to produce an updated `w b`.


#### The `Store` comonad


### License - MIT
```
The MIT License (MIT)
Copyright (c) 2016 Siddharth Bhat

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
``
