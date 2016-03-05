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
        cell = extract grid 
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

### Design

This is an exploration of the Haskell design space to create Cellular Automata.

I finally settled on using [`Comonads`](http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html) to represent the grid space of the cellular automata.
The difference between this implementation and many others in the wild is that this one
has a *finite* grid, which makes writing the instances for `Zipper` and `Comonad` harder. 
I'll be putting up a blog post sometime soon. Until then, enjoy the pretty pictures!


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

### License - MIT
```
The MIT License (MIT)
Copyright (c) 2016 Siddharth Bhat

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
``
