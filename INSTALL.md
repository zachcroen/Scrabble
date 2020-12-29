# Installation and Run Instructions for our Scrabble Game

### Installations:

Currently, there are no necessary installations for this project beyond
the standard installations used in CS3110, Fall 2020.

#### Dependencies:
None beyond those of CS3110


### Build

Run the command `$ make build` in the terminal to create the build for this 
project.

### Play the Game

Run the command `$ make play` in the terminal to start a game.  This initiates
the start sequence of the game.  Next, input a game state json file, followed
by the names of the relevant players.  For example `standard_game.json, Gavin,
Sophie, Zach` loads a standard Scrabble game with three players. 
Next, add words from your tile rack giving commands in the format provided 
in the example.  Type `Quit` to exit at any time. 
Note: Always run `$ make clean` before `$ make play`. If the game can 
not find your file, and you are sure that you are typing it in correctly, 
try exiting out and running `$ make clean`. 