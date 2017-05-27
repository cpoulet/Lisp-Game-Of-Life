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

[![Game_of_life.gif](https://raw.githubusercontent.com/cpoulet/Lisp-Game-Of-Life/master/Game_of_life.gif)](https://raw.githubusercontent.com/cpoulet/Lisp-Game-Of-Life/master/Game_of_life.gif)

Infomations
======

All the compilation information are described in the [PDF](Game_of_life.pdf)

I am using SDL for graphical part.  

Usage: game_of_life --load width height  
         width: width of the grid   
         height: height of the grid
