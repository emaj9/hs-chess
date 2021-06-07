# hs-chess

Welcome! This is chess implemented in Haskell. This is run completely on the
command line, using terminal graphics to display the board and pieces.

### Installation
1. Clone this repo
2. **Using stack** run `stack run --profile` in the repo folder
3. Enjoy your game!

### How to play
2 players play this game together using the machine. This chess game uses [algebraic chess
notation](https://en.wikipedia.org/wiki/Algebraic_notation_(chess)). Each player
takes a turn entering a move. If presented with an invalid input or move, the
game will ask the player to enter a correct move in chess notation.

### Missing features
- No castling
- No taking en passant
- No promotion
