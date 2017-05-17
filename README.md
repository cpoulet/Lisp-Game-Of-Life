The Game of Life by John Conway
======

The Game of Life is not your typical computer game.
It is a 'cellular automaton', and was invented by Cambridge mathematician John Conway.


The Rules
======

* For a cell that is 'alive':
  * Each cell with one or no neighbors dies, as if by solitude.
  * Each cell with four or more neighbors dies, as if by overpopulation.
  * Each cell with two or three neighbors survives.
* For a cell that is 'dead':
  * Each cell with three neighbors becomes 'alive', as if by birth.
  
My Simulation in Lisp
======

[![Game_of_life.gif](https://s10.postimg.org/acpyxo0q1/Game_of_life.gif)](https://postimg.org/image/j7qt86pid/)
