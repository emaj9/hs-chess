{-# LANGUAGE TupleSections #-}
import Data.Char
import Data.Tuple
import Data.Maybe
import Data.List

data PUnit = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
type Piece = (PUnit, Color)
oppositeColor c = case c of
  Black -> White
  White -> Black

type Pos = (Int, Int) --(row, col)
type Square = (Pos, Maybe Piece)
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

findSquare :: Pos -> Board -> Maybe Piece
findSquare (row, col) board = board!!row!!col

findPiecePos :: Piece -> Board -> Int -> Maybe Pos
findPiecePos piece board row = case board of
  x:xs -> case findIndex (== Just piece) x of
    Just x -> Just (row, x)
    Nothing -> findPiecePos piece xs (row+1)
  [] -> Nothing
  
--findPiecePos' :: Board -> Piece -> Maybe Pos
--findPiecePos' board piece = 
  --case find (not . null) . (zip [0..7]) $ (map 
    --(filter (\(col, piece') -> piece' == Just piece)
    -- . zip [0..7] )
    --(board)) of
    --Just (row, (col, _):_) -> Just (row, col)
    --Nothing -> Nothing


pieceToDir :: Piece -> [Dir]
pieceToDir (unit, _) = case unit of
  Bishop -> [up+++right, down+++right, down+++left, up+++left]
  Rook -> [up, down, left, right]
  Pawn -> [(1,0), (2,0), (1,-1), (1,1)]
  Knight -> [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1),(-2, 1), (-1, 2)]
  --[ (x, y) | x <- [-2, -1, 1, 2], y <- [-2, -1, 1, 2], if abs x /= abs y ]
  --king or queen
  _ -> [up, down, right, left, up+++right, down+++right, down+++left, up+++left]

validMoves :: Pos -> Piece -> Board -> [Square]
validMoves (row, col) piece board =
    case piece of
      (Pawn, c) -> 
        let maybeMoves = map (\dir -> pawnMovesFilter (colorPawnMoves dir c) (row, col) board c) (pieceToDir (Pawn, c)) in
          catMaybes maybeMoves
      --Just (Knight, c) -> error"implement knight move"
      piece' ->
        let movesInDirs = allSquaresPerDirs piece' (row, col) board in
          concatMap (`reachableFilter` (snd piece')) movesInDirs

colorPawnMoves :: Dir -> Color -> Dir
colorPawnMoves (row, col) color = 
  if color == White then (row * (-1), col) else (row, col)

pawnMovesFilter :: Dir -> Pos -> Board -> Color -> Maybe Square
pawnMovesFilter moveOption pos board color = 
  let movedPos = pos +++ moveOption in
    if not $ isInBounds movedPos then Nothing else
      let destinationSquare = findSquare movedPos board in
        let destSquareUnoccupied = isNothing destinationSquare in
          case dirAbs moveOption of
            (1, 0) -> if destSquareUnoccupied then Just (movedPos, Nothing) else Nothing
            (2, 0) -> case canTwoStep pos color of 
              True -> if isNothing (findSquare (pos +++ (colorPawnMoves (1,0) Black)) board) && destSquareUnoccupied 
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
reachableFilter :: [Square] -> Color -> [Square]
reachableFilter movesInDir color = 
  -- span will cut the list of all moves in a give dir into
  -- ([All empty squares in that dir], [Squares that are occupied])
  let (validMoves, maybeMoves)= span (\(pos, piece) -> isNothing piece) movesInDir in
    --we then check the head of [squares that are occupied] to see: can we take from the head of this list?
    -- ex if the first non-empty square is an opponent piece, we add this as a "valid" move (a take)
    case maybeMoves of
      [] -> validMoves
      (_, Just (piece', color')):xs | color' /= color -> validMoves ++ [head maybeMoves]
      _ -> validMoves


-- gets list of ALL squares ex [(maybe unit, pos)] for each possible movement option for a unit
allSquaresPerDirs :: Piece -> Pos -> Board -> [[Square]]
allSquaresPerDirs (unit, color) pos board =
  let dirs = pieceToDir (unit, color) in
    if unit == Queen || unit == Bishop || unit == Rook then 
        map (\dir -> squaresInDir dir pos board True) dirs
        else
          map (\dir -> squaresInDir dir pos board False) dirs

-- might seem wasteful to create this list of all pieces in a given direction
-- if the plan is to filter this list until we hit a piece (to find a valid moves)
-- BUT haskell's laziness means we don't *really* construct the full list of pieces in a given dir!
-- Haskell will compute everything just in time, so if we never use the elements after the cut off, there is no cost
squaresInDir :: Dir -> Pos -> Board -> Bool -> [Square]
squaresInDir dir pos board isMultiMove
  | not $ isInBounds (pos +++ dir) = []
  | otherwise =
    if isMultiMove then
      let newPos = pos +++ dir in
        (newPos, findSquare newPos board) : squaresInDir dir newPos board True
    else [(dir+++pos, findSquare (dir+++pos) board)]

--dirUnderAttack :: Dir -> Color -> Pos -> Board -> Bool
dirUnderAttack dir color pos board = 
  error"not done" --reachableFilter [squaresInDir dir pos board] color

--checkCheck :: Board -> Color -> Bool
checkCheck board color =
  --kingPos == Maybe Pos
  let kingPos = findPiecePos (King, color) board 0 in
    if isNothing kingPos then error "King not on board" else 
      let possibleAttackers = map (,color) [King, Pawn, Rook, Bishop, Knight] in
        any (\p -> pieceCanAttackPos (fromJust kingPos) p board) possibleAttackers


pieceCanAttackPos:: Pos -> Piece -> Board -> Bool
pieceCanAttackPos pos piece board = 
  let opColor = oppositeColor $ snd piece in
  case fst piece of
    Pawn ->
      let attackPos'' = if snd piece == White then [(-1, -1), (-1, 1)] else [(1, -1), (1, 1)] in
        let attackPos' = map (+++ pos) attackPos'' in
          let attackPos = filter isInBounds attackPos' in
          let diagSquares = map (`findSquare` board) attackPos in
            elem (Just (Pawn, opColor)) diagSquares
    King -> 
      let attackPos' = map (+++pos) (pieceToDir (King, opColor)) in
        let attackPos = filter isInBounds attackPos' in
          elem (Just (King, opColor)) (map (`findSquare` board) attackPos)
    Queen -> error "bad input"
    _ -> findAttackersOfPos pos piece board
    
findAttackersOfPos pos (unit, color) board =
  let unitsInSight = validMoves pos (unit, color) board in
    let color' = oppositeColor color in
  let potentialAttackers = if unit == Rook || unit == Bishop then [(unit, color'),(Queen, color')] else [(unit, color')] in
  any (\(pos, piece') -> 
    isJust piece' &&  
    elem (fromJust piece') potentialAttackers)
    unitsInSight

--checkCheck :: Board -> Color -> Bool
--checkCheck board color = 
  ----kingPOs == Maybe Pos
  --let kingPos = findPiecePos (King, color) board 0 in
    --if isNothing kingPos then error "King not on board" else 
    ---- dirs :: [[Dir]]
    ---- ex dirs = [ [(1,0), (-1,0), left, right], [diags...]]
    --let dirs = map (\x -> squaresInDir pieceToDir (x, oppositeColor color)) [Rook, Bishop] in
      ----dirSquares :: [ [ [ (pos) piece), (pos, maybe peice)]] ]
      --let dirSquares = map (map (\move -> squaresInDir move (fromJust kingPos) board)) dirs in
        --let dirSquares' = map (`reachableFilter` oppositeColor color) dirSquares in
          --map (map (filter (\x -> snd x == Just (King, color)))) dirSquares'

  --1: look for the king
  --2. us all the dirs

isInBounds :: Pos -> Bool
isInBounds (row, col) = 0 <= row && row <= 7 && 0 <= col && col <= 7 -- && (row /= 0 && col /= 0)




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

testBoard' = [
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Pawn, Black)) [0..7],
  map (\x -> Nothing) [0..7],
  [Nothing, Nothing, Nothing, Just (King, Black), Nothing, Nothing, Nothing, Nothing],
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Queen, White)) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7]
  ]

testBoard = [
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Pawn, White)) [0..7],
  map (\x -> Nothing) [0..7],
  [Nothing, Nothing, Nothing, Just (King, Black), Nothing, Nothing, Nothing, Nothing],
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Knight, White)) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7]
  ]