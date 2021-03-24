import Data.Char
import Data.Tuple
import Data.Maybe

data PUnit = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
type Piece = (PUnit, Color)
oppositeColor c = case c of
  Black -> White
  White -> Black

type Pos = (Int, Int) --(row, col)
type Board = [[Maybe Piece]]

type Dir = Pos
(+++) :: Dir -> Dir -> Dir
(d0, d1) +++ (d2, d3) = (d0 + d2, d1 + d3)
(***) :: Dir -> Dir -> Dir
(d0, d1) *** (d2, d3) = (d0 * d2, d1 * d3)
dirAbs :: Dir -> Dir
dirAbs (row, col) = (abs row, abs col)
up, down, left, right :: Dir
up = (1, 0)
down = (-1, 0)
left = (0, -1)
right = (0, 1)

infixl 5 +++

--for printing the board
showSquare :: Maybe Piece -> Char
showSquare square = case square of
  Just a -> showPiece a
  Nothing -> '_'

showPiece :: Piece -> Char
showPiece (unit, color) = case color of
  White -> head $ if unit == Knight then "N" else show unit
  Black -> toLower $ head $ if unit == Knight then "N" else show unit

printableBoard :: Board -> [String]
printableBoard board =
  map (\row -> map showSquare row) board

--

findPiece :: Pos -> Board -> Maybe Piece
findPiece (row, col) board = board!!row!!col

pieceToDir :: Piece -> [Dir]
pieceToDir (unit, _) = case unit of
  Bishop -> [up+++right, down+++right, down+++left, up+++left]
  Rook -> [up, down, left, right]
  Pawn -> [(1,0), (2,0), (1,-1), (1,1)]
  Knight -> [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1),(-2, 1), (-1, 2)]
  --[ (x, y) | x <- [-2, -1, 1, 2], y <- [-2, -1, 1, 2], if abs x /= abs y ]
  --king or queen
  _ -> [up, down, right, left, up+++right, down+++right, down+++left, up+++left]

--validMoves :: Pos -> Board -> [(Pos, Maybe Piece)]
validMoves (row, col) board color =
  let piece = findPiece (row, col) board in
    case piece of
      Just (Pawn, c) -> 
        let maybeMoves = map (\dir -> pawnMovesFilter (colorPawnMoves dir color) (row, col) board color) (pieceToDir (Pawn, color)) in
          map maybeToList maybeMoves
      --Just (Knight, c) -> error"implement knight move"
      Just piece ->
        let allMoves = allSquaresInDirs piece (row, col) board in
          reachableSquares allMoves piece
      Nothing -> error "No piece at this location in call validMoves"

colorPawnMoves :: Dir -> Color -> Dir
colorPawnMoves (row, col) color = 
  if color == White then (row * (-1), col) else (row, col)

pawnMovesFilter :: Dir -> Pos -> Board -> Color -> Maybe (Pos, Maybe Piece)
pawnMovesFilter moveOption pos board color = 
  let movedPos = pos +++ moveOption in
    if not $ isInBounds movedPos then Nothing else
      let destinationSquare = findPiece movedPos board in
        let destSquareUnoccupied = isNothing destinationSquare in
          case dirAbs moveOption of
            (1, 0) -> if destSquareUnoccupied then Just (movedPos, Nothing) else Nothing
            (2, 0) -> case canTwoStep pos color of 
              True -> if isNothing (findPiece (pos +++ (colorPawnMoves (1,0) Black)) board) && destSquareUnoccupied 
                  then Just (movedPos, Nothing) else  Nothing
              False -> Nothing
            (1, 1) -> case destinationSquare of 
                      Just (_, c) | c == oppositeColor color -> Just (movedPos, destinationSquare)
                      _ -> Nothing
            _ -> error ("no pattern for moveoptions" ++ (show $ fst $dirAbs moveOption) ++ (show $ fst $dirAbs moveOption))
                      
canTwoStep :: Pos -> Color -> Bool
canTwoStep pos color = case color of
  White -> fst pos == 6
  Black -> fst pos == 1

-- takes a list all moves in the correct direction for a piece and returns only the possible moves, 
-- this means moves that end on empty squares and capturing moves are kept in the output list
reachableSquares :: [[(Pos, Maybe Piece)]] -> Piece -> [[(Pos, Maybe Piece)]]
reachableSquares allMoves piece = 
  -- span will cut the list of all moves in a give dir into
  -- ([All empty squares in that dir], [Squares that are occupied])
  let emptyAndOccupiedSquares = map (span (\(pos, piece) -> isNothing piece)) allMoves in
    --we then check the head of [squares that are occupied] to see: can we take from the head of this list?
    -- ex if the first non-empty square is an opponent piece, we add this as a "valid" move (a take)
    map (\(nothingSquares, justSquares) -> case justSquares of
            (_, Just (piece', color)) : xs -> 
              if color /= snd piece then nothingSquares ++ [head justSquares] else nothingSquares
            _ -> nothingSquares)
    emptyAndOccupiedSquares


-- gets list of ALL squares ex [(maybe unit, pos)] for each possible movement option for unit
allSquaresInDirs :: Piece -> Pos -> Board -> [[(Pos, Maybe Piece)]]
allSquaresInDirs (unit, color) pos board =
  let dirs = pieceToDir (unit, color) in
    case unit of
    Knight -> singleMoveCheck
    King -> singleMoveCheck
    _ -> map (\dir -> getSquaresInDir dir pos board) dirs
    where singleMoveCheck = map (\dir -> let newPos = pos +++ dir in [(newPos, findPiece newPos board)]) (pieceToDir (unit, color))

checkCheck :: Board -> Color -> Bool
checkCheck board color = 
  error"unimplemented"
  --1: look for the king
  --2. us all the dirs

isInBounds :: Pos -> Bool
isInBounds (row, col) = 0 <= row && row <= 7 && 0 <= col && col <= 7 -- && (row /= 0 && col /= 0)

-- might seem wasteful to create this list of all pieces in a given direction
-- if the plan is to filter this list until we hit a piece (to find a valid moves)
-- BUT haskell's laziness means we don't *really* construct the full list of pieces in a given dir!
-- Haskell will compute everything just in time, so if we never use the elements after the cut off, there is no cost
getSquaresInDir :: Dir -> Pos -> Board -> [(Pos, Maybe Piece)]
getSquaresInDir dir pos board
  | not $ isInBounds (pos +++ dir) = []
  | otherwise =
    let newPos = pos +++ dir in
     (newPos, findPiece newPos board) : getSquaresInDir dir newPos board



main = do
  --pretty prints the board
  mapM_ print $ printableBoard testBoard


initBoard = [
  map (\x -> Just (x, White)) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook],
  map (\x -> Just (Pawn, White)) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Pawn, Black)) [0..7],
  map (\x -> Just (x, Black)) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
  ]

testBoard = [
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Pawn, Black)) [0..7],
  map (\x -> Nothing) [0..7],
  [Nothing, Nothing, Nothing, Just (King, Black), Nothing, Nothing, Nothing, Nothing],
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Pawn, White)) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7]
  ]
