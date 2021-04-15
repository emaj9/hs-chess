{-# LANGUAGE TupleSections #-}
import Data.Char
import Data.Tuple
import Data.Maybe
import Data.List

data Unit = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
type Piece = (Unit, Color)
oppositeColor c = case c of
  Black -> White
  White -> Black

-- Move = Piece maybe(Row | Col) (mX) destination
data Move = Move Unit (Maybe Int) (Maybe Int) Bool Pos (Maybe Unit) | CastlesKing | CastlesQueen deriving Show
type Parser a = String -> Maybe (a, String)


type Pos = (Int, Int) --(row, col)
type Square = (Pos, Maybe Piece)
type Board = [Rank]
type Rank = [Maybe Piece]
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

parserCombinator :: Parser a -> Parser (Maybe a)
parserCombinator parser str = 
  case parser str of
    Nothing -> Just (Nothing, str)
    Just (a, str) -> Just (Just a, str)

parserOrElse :: Parser a -> Parser a -> Parser a
parserOrElse p1 p2 str = 
  case p1 str of
    Nothing -> p2 str
    Just (x, str) -> Just (x, str)


parseMove :: Parser Move
parseMove str = case str of
  "0-0" -> Just (CastlesKing, "")
  "0-0-0" -> Just (CastlesQueen, "")
  _ -> do 
    parserOrElse longMove shortMove str
  where 
    longMove :: Parser Move
    longMove str = do
      (moveUnit, str) <- charToUnit str
      (startCol, str) <- parserCombinator (charToCol) str
      (startRow, str) <- parserCombinator (charToRow) str
      (takes, str)    <- charToTakes str
      (destCol, str) <- charToCol str
      (destRow, str) <- charToRow str
      (promotion, str)<- parserCombinator charsToPromo str
      Just (Move moveUnit startRow startCol takes (destRow, destCol) promotion, str)
    shortMove str = do
      (moveUnit, str) <- charToUnit str
      (takes, str)    <- charToTakes str
      (destCol, str) <- charToCol str
      (destRow, str) <- charToRow str
      (promotion, str)<- parserCombinator charsToPromo str
      Just (Move moveUnit Nothing Nothing takes (destRow, destCol) promotion, str)

validateMove :: Color -> Board -> Move -> Maybe Move
validateMove color board move = 
  case move of
    --Move unit (Just row) (Just col) takes (dRow, dCol) promo -> 
      --if isNothing $ lookup (dRow, dCol) $ validMovesForPiece (row, col) (unit, color) board then
        --Nothing else Just move
    Move Pawn row col False (dRow, dCol) promo -> do
      let moves = map (+++ (dRow, dCol)) $ if color == Black then [(-1, 0), (-2, 0)] else [(1,0), (2,0)]
      let squaresToCheck = map (\p -> (p, p `findSquare` board)) moves 
      ((row, col), mPiece) <- find (\(pos, x) -> case x of
                  Just (Pawn, color') -> color' == color
                  Nothing -> False)
                squaresToCheck
      Just (Move Pawn (Just row) (Just col) False (dRow, dCol) Nothing)
      
    Move unit row col takes (dRow, dCol) promo ->
      case unitCanTakePos (dRow, dCol) (unit, oppositeColor color) board of
        [] -> Nothing
        [((r, c), piece')] -> Just (Move unit (Just r) (Just c) takes (dRow, dCol) promo)
        xs -> case (row, col) of
          (Nothing, Nothing) -> Nothing
          (Just x, Just y) -> (\_ -> Move unit (Just x) (Just y) takes (dRow, dCol) promo) <$> lookup (x, y) xs
          (Just x, Nothing) -> case filter (\((x', y), piece) -> x == x') xs of
            [((r, c), piece')] -> Just (Move unit (Just r) (Just c) takes (dRow, dCol) promo )
          (Nothing, Just y) -> case filter (\((x, y'), piece) -> y == y') xs of
            [((r, c), piece')] -> Just (Move unit (Just r) (Just c) takes (dRow, dCol) promo )
            _ ->  Nothing
    --(unit, Nothing, Nothing, takes, (dCol, dRow), promo) | unit == Pawn -> error"unimp"



charToUnit :: String -> Maybe (Unit, String)
charToUnit str = case str of
  "" -> Nothing
  x:xs -> case x of
    'B' -> Just (Bishop, xs)
    'K' -> Just (King  , xs)
    'R' -> Just (Rook  , xs)
    'Q' -> Just (Queen , xs)
    'N' -> Just (Knight, xs)
    _ -> Just (Pawn, x:xs)
  
charToCol :: Parser Int
charToCol str = case str of
  "" -> Nothing
  x:xs -> case x of
    x | ord 'a' <= ord x && ord x <= ord 'h' -> Just ((ord x - ord 'a'), xs)
    _ -> Nothing

charToRow :: Parser Int
charToRow str = case str of
  "" -> Nothing
  x:xs -> case x of
    x |  ord '1' <= ord x && ord x <= ord '8' -> Just ((7 - ord x + ord '1'), xs)
    _ -> Nothing

charToTakes :: Parser Bool
charToTakes str = case str of
  "" -> Nothing
  'x':xs  -> Just (True, xs)
  _ -> Just (False, str)

charsToPromo :: Parser Unit
charsToPromo str = case str of
  '=':xs -> charToUnit xs 
  _ -> Nothing

--for printing the board
showSquare :: Maybe Piece -> Char
showSquare square = case square of
  Just a -> showPiece a
  Nothing -> '_'

showPiece :: Piece -> Char
showPiece (unit, color) = case color of
  White -> head $ if unit == Knight then "N" else show unit
  Black -> toLower $ head $ if unit == Knight then "N" else show unit

printableBoard :: Board -> Int -> [String]
printableBoard board x =
  if x == 8 then [] else
    (show x ++ "|" ++ map showSquare (board!!x) ++ "|") : printableBoard board (x+1)

--

findSquare :: Pos -> Board -> Maybe Piece
findSquare (row, col) board = board!!row!!col

findSquaresOfPiece :: Piece -> Board -> Int -> [Square]
findSquaresOfPiece piece board row = case board of
  x:xs -> case elemIndices (Just piece) x of
    l -> map (\x -> ((row, x), Just piece)) l ++ findSquaresOfPiece piece xs (row+1)
  [] -> []

--findSquaresOfPiece :: Piece -> Board -> Int -> [Square]
--findSquaresOfPiece piece board row = case board of
  --x:xs -> case elemIndices (Just piece) x of
    --l -> map (\x -> ((row, x), Just piece)) l ++ findSquaresOfPiece piece board (row+1)
  --[] -> []
  
pieceToDir :: Piece -> [Dir]
pieceToDir (unit, _) = case unit of
  Bishop -> [up+++right, down+++right, down+++left, up+++left]
  Rook -> [up, down, left, right]
  Pawn -> [(1,0), (2,0), (1,-1), (1,1)]
  Knight -> [(1, 2), (2, 1), (2, -1), (1, -2), (-1, -2), (-2, -1),(-2, 1), (-1, 2)]
  --[ (x, y) | x <- [-2, -1, 1, 2], y <- [-2, -1, 1, 2], if abs x /= abs y ]
  --king or queen
  _ -> [up, down, right, left, up+++right, down+++right, down+++left, up+++left]

colorOccupiedSqs :: Board -> Int -> Int -> Color -> [(Pos, Piece)]
colorOccupiedSqs board x y c
  | x > 7 = []
  | y > 7 = colorOccupiedSqs board (x + 1) 0 c
  | isJust (board !! x !! y) && snd (fromJust (board!!x!!y)) == c
    = ((x, y), fromJust (board !! x !! y)) : colorOccupiedSqs board x (y + 1) c
  | otherwise = colorOccupiedSqs board x (y + 1) c

legalMoves ::  Pos-> Piece -> Board -> [Square]
legalMoves  (row, col) piece board =
  moves
  where 
    moves = case piece of
      (Pawn, c) -> 
        let maybeMoves = map (\dir -> pawnMovesFilter (colorPawnMoves dir c) (row, col) board c) (pieceToDir (Pawn, c)) in
          catMaybes maybeMoves
      piece' ->
        let movesInDirs = allSquaresPerDirs piece' (row, col) board in
          concatMap (`reachableFilter` (snd piece')) movesInDirs

filterSelfCheck ::  Pos-> Piece -> Board -> [Square] -> [Square]
filterSelfCheck (row, col) piece board  = 
  filter (\(pos, mPiece) -> not $ checkCheck (move board ((row, col), Just piece) pos) (snd piece))

validMovesForPiece' :: Pos -> Board -> [Square]
validMovesForPiece' pos board = 
  let piece = findSquare pos board in
    if isNothing piece then error"no piece at this pos!!" else
      validMovesForPiece pos (fromJust piece) board  
validMovesForPiece ::  Pos-> Piece -> Board -> [Square]
validMovesForPiece pos piece board = 
  --filterSelfCheck pos piece board $ 
  legalMoves pos piece board

--validMovesForColor :: Color -> Board -> [Square]
validMovesForColor c b = 
 let pieces = colorOccupiedSqs b 0 0 c in
    concatMap (\(pos, piece) -> validMovesForPiece pos piece b) pieces

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
              True -> if isNothing (findSquare (pos +++ (colorPawnMoves (1,0) color)) board) && destSquareUnoccupied 
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

checkCheck :: Board -> Color -> Bool
checkCheck board color =
  let kingPos = fst (head $ findSquaresOfPiece (King, color) board 0) in
      not $ null $ findAttackersOfPos kingPos color board

--find all the attackers of the opposite color at the given pos
findAttackersOfPos :: Pos -> Color -> Board -> [Square]
findAttackersOfPos pos color board = 
  let possibleAttackers = [King, Pawn, Rook, Bishop, Knight] in
    concatMap (\u -> unitCanTakePos pos (u, color) board) possibleAttackers

-- given an input piece, can a piece of that unit take the given position?
-- returns a list of all the sqaures with units that can do that
unitCanTakePos:: Pos -> Piece -> Board -> [Square]
unitCanTakePos pos (unit, color) board = 
  let opColor = oppositeColor color in
  case unit of
    Pawn ->
      let attackPos'' = if color == White then [(-1, -1), (-1, 1)] else [(1, -1), (1, 1)] in
        let attackPos' = map (+++ pos) attackPos'' in
          let attackPos = filter isInBounds attackPos' in
            let diagSquares = map (\pos -> (pos, pos `findSquare` board)) attackPos in
              filter (\(x, y) -> y == Just (Pawn, opColor)) diagSquares
    King -> 
      let attackPos' = map (+++pos) (pieceToDir (King, opColor)) in
        let attackPos = filter isInBounds attackPos' in
          filter (\(x, y) -> y == Just (King, opColor)) (map (\pos -> (pos, pos `findSquare` board)) attackPos)
    Knight -> 
      let attackPos' = map (+++pos) (pieceToDir (Knight, opColor)) in
        let attackPos = filter isInBounds attackPos' in
          filter (\(x, y) -> y == Just (Knight, opColor)) (map (\pos -> (pos, pos `findSquare` board)) attackPos)
    Queen -> concatMap (\u -> unitCanTakePos pos (u, color) board) [Bishop, Rook]
    _ -> findRunnerAttackers pos (unit, opColor) board
    
findRunnerAttackers :: Pos -> Piece -> Board -> [Square]
findRunnerAttackers pos (unit, color) board =
  let opColor = oppositeColor color in
  let unitsInSight = legalMoves pos (unit, color) board in
    let potentialAttackers = if unit == Rook || unit == Bishop then [(unit, opColor),(Queen, opColor)] else [(unit, opColor)] in
    filter (\(pos', mPiece) -> 
      isJust mPiece &&  
      elem (fromJust mPiece) potentialAttackers)
      unitsInSight

replaceSquare :: [Maybe Piece] -> Int -> Maybe Piece -> [Maybe Piece]
replaceSquare rank file newPiece =
  let (r1, _:r2) = splitAt file rank in
    r1 ++ newPiece : r2

  
--move :: Board -> Move -> Board
--move board (unit, mStartCol, mStartRow, attacking, (destCol, destRow)) = 
move :: Board -> Square -> Pos -> Board
move board ((rank, file), mover) (destR, destF) =
  let mover' = fromMaybe (error "no piece at sq") mover in
    let (r1, changeRank:r2) = splitAt rank board in
      let board' = r1 ++ replaceSquare changeRank file Nothing : r2 in
        let (r1', changeRank':r2') = splitAt destR board' in
          r1' ++ (replaceSquare changeRank' destF mover : r2')

isInBounds :: Pos -> Bool
isInBounds (row, col) = 0 <= row && row <= 7 && 0 <= col && col <= 7 -- && (row /= 0 && col /= 0)

mainLoop board turnColor gameComplete =
  if gameComplete then error"game is over" else
    do
      putStrLn " =========="
      printBoard board
      putStrLn " =========="
      putStrLn "  abcdefgh"
      putStrLn (show turnColor ++ "'s move")
      input <- getLine 
      Move unit (Just startRow) (Just startCol) takes (destRow, destCol) promo <- 
        case parseMove input >>= validateMove turnColor board . fst of
          Nothing -> error":("
          Just m -> pure m

      let board' = move board ((startRow, startCol), Just (unit, turnColor)) (destRow, destCol)
      mainLoop board' (oppositeColor turnColor) False


main = do
  --pretty prints the board
  mainLoop initb White False
  

printBoard board = mapM_ putStrLn $ printableBoard board 0

initb = [
  map (\x -> Just (x, Black)) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook],
  map (\x -> Just (Pawn, Black)) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Just (Pawn, White)) [0..7],
  map (\x -> Just (x, White)) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]
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

sc = [
  map (\x -> if x == 4 then Just (King, Black) else Nothing) [0..7],
  map (\x -> if x == 4 then Just (Pawn, Black) else Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> if x == 4 then Just (Rook, White) else Nothing) [0..7],
  map (\x -> if x == 4 then Just (King, White) else Nothing) [0..7]
  ]

pt = [
  map (\x -> Nothing) [0..6] ++ [Just (King, Black)],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  [Nothing, Nothing, Nothing, Nothing, Just (Knight, Black), Nothing, Nothing, Nothing],
  [Nothing, Nothing, Nothing, Just (Pawn, White), Nothing, Nothing, Nothing, Nothing],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..7],
  map (\x -> Nothing) [0..6] ++ [Just (King, White)]
  ]