import Data.Char
import Data.Maybe

data PUnit = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data PColor = White | Black deriving (Eq, Show)
type Piece = (PUnit, PColor)

type Pos = (Int, Int) --(row, col)
type Board = [[Maybe Piece]]

type Dir = Pos
(+++) :: Dir -> Dir -> Dir
(d0, d1) +++ (d2, d3) = (d0 + d2, d1 + d3)
up, down, left, right :: Dir
up = (1, 0)
down = (-1, 0)
left = (0, -1)
right = (0, 1)

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
  Queen -> [up, down, right, left, up+++right, down+++right, down+++left, up+++left]
  Bishop -> [up+++right, down+++right, down+++left, up+++left]
  Rook -> [up, down, left, right]


--validMoves :: Pos -> Board -> [(Pos, Maybe Piece)]
-- TODO: implement the valid moves for knight, king, pawn
validMoves (row, col) board =
  let piece = findPiece (row, col) board in
    case piece of
      Just (Pawn, c) -> error"implement pawn move"
      Just (Knight, c) -> error"implement knight move"
      Just piece ->
        -- gets list of ALL (unit, pos) for each possible movement directions for unit
        -- dirMoves :: [[(Pos, Maybe Piece)]]
        let dirMoves = map (\dir -> getSquaresInDir dir (row, col) board) (pieceToDir piece) in
        -- now let's split the list of squares in a given dir based on empty or occupied
        -- eAndOMoves :: [([(Pos, mPiece)], [(Pos, mPiece)])]
        let emptyAndOccupiedMoves = map (\dirList -> span (\(pos, piece) -> isNothing piece) dirList) dirMoves in
        -- now let's look at the tuples of empty and pieces, and see if the head of pieces is a takeable unit
        map (\(emptyMoves, unitMoves) -> case unitMoves of
                (_, Just (piece, color)) : xs -> if color == Black
                                            then (head unitMoves) : emptyMoves
                                            else emptyMoves
                _ -> emptyMoves)
        emptyAndOccupiedMoves
      Nothing -> error "No piece at this location in call validMoves"


-- might seem wasteful to create this list of all pieces in a given direction
-- if the plan is to filter this list until we hit a piece (to find a valid moves)
-- BUT haskell's laziness means we don't *really* construct the full list of pieces in a given dir!
-- Haskell will compute everything just in time, so if we never use the elements after the cut off, there is no cost
getSquaresInDir :: Dir -> Pos -> Board -> [(Pos, Maybe Piece)]
getSquaresInDir dir (row, col) board
  | 0 >= row || row >= 7 || 0 >= col || col >= 7 = []
  | otherwise =
    let newPos = (row, col) +++ dir in
     (newPos, findPiece newPos board) : (getSquaresInDir dir newPos board)



main = do
  --pretty prints the board
  mapM_ print $ printableBoard initBoard
  print $ findPiece (1, 1) initBoard


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
  map (\x -> Just (Pawn, White)) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  [Nothing, Nothing, Nothing, Just (Bishop, White), Nothing, Nothing, Nothing, Nothing],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Pawn, Black)) [0..7]
  ]
