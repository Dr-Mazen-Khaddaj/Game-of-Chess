module GameLogic
    ( possibleMovements
    , checkPromotion
    , checkKingAfterMove
    , checkCastleMove
    , checkEnPassantMove
    , checkMovement
    , changePlayer
    , checkGameOver
    , updateSkippedSquare
    , makeMove
    , updateKilledPieces
    , prepareEnPassantMove
    , kingNotInCheck
    ) where

import Control.Monad (void)
import Control.Monad.State (MonadState, get, gets, modify)
import Types
    ( Board
    , CastlingInfo
        ( getKingPath
        , getRookDestination
        , getRookPath
        , getRookPosition
        )
    , Game
        ( currentPlayer
        , getBoard
        , getCastlingInfo
        , killedBlackPieces
        , killedWhitePieces
        , movedPieces
        , opponent
        , skippedSquare
        )
    , Piece (Bishop, King, Knight, Pawn, Queen, Rook)
    , Player (..)
    , Position
    , Skipped (None, Only)
    , Square (..)
    )

import qualified Data.Map as Map

{- FOURMOLU_DISABLE -}

-- | Functions | --
--
{- Make Move -}
makeMove :: MonadState Game m => Position -> Position -> m Square
makeMove position destination = do
    board <- gets getBoard
    let pieceToMove      = board Map.! position
    let pieceToReplace   = board Map.! destination
    let updatedBoard     =        Map.insert destination pieceToMove    . Map.insert position Empty       $ board
    newMovedPieces      <- gets $ Map.insert destination pieceToReplace . Map.insert position pieceToMove . movedPieces
    modify (\ game -> game { movedPieces  =  newMovedPieces
                           , getBoard     =  updatedBoard })
    return pieceToReplace
--
{- Update Skipped Square -}
updateSkippedSquare :: MonadState Game m => Position -> Position -> m ()
updateSkippedSquare position destination = do
    player          <- gets currentPlayer
    pieceToMove     <- gets $ (Map.! position) . getBoard
    let isPawn      =  pieceToMove `elem` [White Pawn, Black Pawn]
    let deltaY      =  abs $ snd position - snd destination
    let stepBack    =  case player of Player1 -> (+) (-1) ; Player2 -> (+) 1
    if isPawn && deltaY == 2
        then modify (\ game -> game {skippedSquare = Only (fmap stepBack destination, destination)})
        else modify (\ game -> game {skippedSquare = None})
--
{- Update Killed Pieces -}
updateKilledPieces :: MonadState Game m => Square -> m ()
updateKilledPieces replacedPiece = do
    case replacedPiece of
        White _ -> do
            killedPieces <- gets killedWhitePieces
            modify (\ game -> game {killedWhitePieces = replacedPiece : killedPieces})
        Black _ -> do
            killedPieces <- gets killedBlackPieces
            modify (\ game -> game {killedBlackPieces = replacedPiece : killedPieces})
        Empty -> return ()
--
{- Change Player -}
changePlayer :: MonadState Game m => m ()
changePlayer = do
    player <- gets currentPlayer
    case player of
        Player1 -> modify (\ game -> game {currentPlayer = Player2 , opponent = Player1})
        Player2 -> modify (\ game -> game {currentPlayer = Player1 , opponent = Player2})
--
{- Check King -}
checkKingAfterMove :: MonadState Game m => Position -> Position -> m Bool
checkKingAfterMove position destination = do
    player  <- gets currentPlayer
    board   <- gets getBoard
    let pieceToMove = board Map.! position
    let doMove = Map.insert destination pieceToMove . Map.insert position Empty
    pure . kingNotInCheck player . doMove $ board
--
kingNotInCheck :: Player -> Board -> Bool
kingNotInCheck player board = Map.null $ possibleDestination board kingPosition
    where kingPosition = case player of
            Player1 -> head $ Map.keys $ Map.filter (== White King) board
            Player2 -> head $ Map.keys $ Map.filter (== Black King) board
--
-- Takes a board, and a final position (destination), and returns all possible moves to that destination --
possibleDestination :: Board -> Position -> Map.Map Position Square
possibleDestination board position = Map.filterWithKey validMove board
    where
        validMove k pieceToMove = k /= position && pieceToMove /= Empty && checkMovement pieceToMove board k position
--
{- Free to move -}
-- Takes the position of a piece and returns whether possible/legal move is found or not.
possibleMovements :: MonadState Game m => Position -> m Bool
possibleMovements position = do
    board <- gets getBoard
    let pieceToMove    = board Map.! position
    let checkMove k _  = k /= position && checkMovement pieceToMove board position k
    let checkKing k _  = checkKingAfterMove position k
    let enPassant k _  = checkEnPassantMove position k
    let possibleMoves  = Map.filterWithKey checkMove board
    foundEnPassantMove <- (||) . or <$> sequence (Map.mapWithKey enPassant board)
    foundEnPassantMove . or <$> sequence (Map.mapWithKey checkKing possibleMoves)
--
{- Promotion -}
checkPromotion :: MonadState Game m => m Bool
checkPromotion = do
    player  <- gets currentPlayer
    board   <- gets getBoard
    case player of
        Player1 -> return $ or $ Map.mapWithKey (\ (_, y) s -> y == 8 && s == White Pawn) board
        Player2 -> return $ or $ Map.mapWithKey (\ (_, y) s -> y == 1 && s == Black Pawn) board
--
{- Castling -}
-- Since this is a special, 2 step move, I'm doing the checking + part of the move in the same function.
-- If valid, do First part of the move (Rook Move) and return True.
checkCastleMove :: MonadState Game m => Position -> Position -> m Bool
checkCastleMove kingPosition kingDestination = do
    game <- get
    case Map.lookup (kingPosition, kingDestination) $ getCastlingInfo game of
        Nothing -> pure False
        Just _  -> do
            rookPosition    <- gets $ getRookPosition    . (Map.! (kingPosition, kingDestination)) . getCastlingInfo
            rookDestination <- gets $ getRookDestination . (Map.! (kingPosition, kingDestination)) . getCastlingInfo
            rookPath        <- gets $ getRookPath        . (Map.! (kingPosition, kingDestination)) . getCastlingInfo
            kingPath        <- gets $ getKingPath        . (Map.! (kingPosition, kingDestination)) . getCastlingInfo
            board           <- gets getBoard
            let clearPath   = all ((== Empty) . (board Map.!)) rookPath
            kingPathNotInCheck <- mapM (checkKingAfterMove kingPosition) kingPath
            didNotMoveKing <- gets $ Map.notMember kingPosition . movedPieces
            didNotMoveRook <- gets $ Map.notMember rookPosition . movedPieces
            if and (didNotMoveKing : didNotMoveRook : clearPath : kingPathNotInCheck)
                then makeMove rookPosition rookDestination >> return True
                else return False
--
{- En passant -}
checkEnPassantMove :: MonadState Game m => Position -> Position -> m Bool
checkEnPassantMove position destination = do
    board            <- gets getBoard
    skippedSquare    <- gets skippedSquare
    let pieceToMove  =  board Map.! position
    let isPawn       =  pieceToMove `elem` [White Pawn, Black Pawn]
    case skippedSquare of
        Only (posEmpty, posPawn) -> if isPawn && destination == posEmpty
            then validateEnPassantMove posPawn position destination
            else return False
        None  -> return False
--
validateEnPassantMove :: MonadState Game m => Position -> Position -> Position -> m Bool
validateEnPassantMove posPawn position destination = do
    player  <- gets currentPlayer
    board   <- gets getBoard
    let enemyPiece     = board Map.! posPawn
    let pieceToMove    = board Map.! position
    let modifiedBoard  = Map.insert destination enemyPiece  . Map.insert posPawn Empty $ board
    let finalBoard     = Map.insert destination pieceToMove . Map.insert posPawn Empty . Map.insert position Empty $ board
    let checkMove      = checkMovement pieceToMove modifiedBoard position destination
    let checkKing      = kingNotInCheck player finalBoard
    return $ checkMove && checkKing
--
prepareEnPassantMove :: MonadState Game m => m ()
prepareEnPassantMove = do
    skippedSquare <- gets skippedSquare
    case skippedSquare of
        Only (posEmpty, posPawn) -> void $ makeMove posPawn posEmpty
        None -> error "Can't prepare En passant move!"
--
{- check if Game is over -}
checkGameOver :: MonadState Game m => m Bool
checkGameOver = do
    player  <- gets currentPlayer
    board   <- gets getBoard
    let player'sPiece  (White _)  =  player == Player1
        player'sPiece  (Black _)  =  player == Player2
        player'sPiece   Empty     =  False
    let positions = Map.keys . Map.filter player'sPiece $ board
    not . or <$> mapM possibleMovements positions
--
{- Check Move -}
checkMovement :: Square -> Board -> Position -> Position -> Bool
-- Pawn --
checkMovement (White Pawn) board (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2-1           = finalDestination == Empty
    | x1 == x2 && y1 == 2 && y2 == 4   = path == [Empty, Empty]
    | abs (x1 - x2) == 1 && y1 == y2-1 = case finalDestination of Black _ -> True; _ -> False
    | otherwise = False
    where
        path@(finalDestination:_) = [board Map.! (x2, y2-n) | n <- [0,1]]

checkMovement (Black Pawn) board (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2+1           = finalDestination == Empty
    | x1 == x2 && y1 == 7 && y2 == 5   = path == [Empty, Empty]
    | abs (x1 - x2) == 1 && y1 == y2+1 = case finalDestination of White _ -> True; _ -> False
    | otherwise = False
    where
        path@(finalDestination:_) = [board Map.! (x2, y2+n) | n <- [0,1]]

-- Rook --
checkMovement (White Rook) board (x1, y1) (x2, y2)
    | x1 == x2 = checkPath (White Rook) pathX finalDestination
    | y1 == y2 = checkPath (White Rook) pathY finalDestination
    | otherwise = False
    where
        finalDestination = board Map.! (x2 , y2)
        pathX = [board Map.! (x2 , y2 - n * signum (y2 - y1)) | n <- [1 .. abs (y2 - y1) - 1]]
        pathY = [board Map.! (x2 - n * signum (x2 - x1) , y2) | n <- [1 .. abs (x2 - x1) - 1]]

checkMovement (Black Rook) board (x1, y1) (x2, y2)
    | x1 == x2 = checkPath (Black Rook) pathX finalDestination
    | y1 == y2 = checkPath (Black Rook) pathY finalDestination
    | otherwise = False
    where
        finalDestination = board Map.! (x2 , y2)
        pathX = [board Map.! (x2 , y2 - n * signum (y2 - y1)) | n <- [1 .. abs (y2 - y1) - 1]]
        pathY = [board Map.! (x2 - n * signum (x2 - x1) , y2) | n <- [1 .. abs (x2 - x1) - 1]]

-- Bishop --
checkMovement (White Bishop) board (x1, y1) (x2, y2)
    | abs (y2 - y1) == abs (x2 - x1) = checkPath (White Bishop) pathX finalDestination
    | otherwise = False
    where
        (finalDestination : pathX) = [board Map.! (x2 - n * signum (x2 - x1) , y2 - n * signum (y2 - y1)) | n <- [0 .. abs (y2 - y1) - 1]]

checkMovement (Black Bishop) board (x1, y1) (x2, y2)
    | abs (y2 - y1) == abs (x2 - x1) = checkPath (Black Bishop) pathX finalDestination
    | otherwise = False
    where
        (finalDestination : pathX) = [board Map.! (x2 - n * signum (x2 - x1) , y2 - n * signum (y2 - y1)) | n <- [0 .. abs (y2 - y1) - 1]]

-- Knight --
checkMovement (White Knight) board (x1, y1) (x2, y2)
    | abs (x2 - x1) == 1 && abs (y2 - y1) == 2 = checkPath (White Knight) [] finalDestination
    | abs (x2 - x1) == 2 && abs (y2 - y1) == 1 = checkPath (White Knight) [] finalDestination
    | otherwise = False
    where
        finalDestination = board Map.! (x2, y2)

checkMovement (Black Knight) board (x1, y1) (x2, y2)
    | abs (x2 - x1) == 1 && abs (y2 - y1) == 2 = checkPath (Black Knight) [] finalDestination
    | abs (x2 - x1) == 2 && abs (y2 - y1) == 1 = checkPath (Black Knight) [] finalDestination
    | otherwise = False
    where
        finalDestination = board Map.! (x2, y2)

-- Queen --
checkMovement (White Queen) board p1 p2 =
    checkAsBishop || checkAsRook
    where
        checkAsBishop = checkMovement (White Bishop) board p1 p2
        checkAsRook   = checkMovement (White Rook)   board p1 p2

checkMovement (Black Queen) board p1 p2 =
    checkAsBishop || checkAsRook
    where
        checkAsBishop = checkMovement (Black Bishop) board p1 p2
        checkAsRook   = checkMovement (Black Rook)   board p1 p2

-- King --
checkMovement (White King) board (x1, y1) (x2, y2)
    | deltaPos `elem` [(1,0), (0,1), (1,1)] = checkPath (White King) [] finalDestination
    | otherwise = False
    where
        deltaPos = (abs (x2 - x1) , abs (y2 - y1))
        finalDestination = board Map.! (x2, y2)

checkMovement (Black King) board (x1, y1) (x2, y2)
    | deltaPos `elem` [(1,0), (0,1), (1,1)] = checkPath (Black King) [] finalDestination
    | otherwise = False
    where
        deltaPos = (abs (x2 - x1) , abs (y2 - y1))
        finalDestination = board Map.! (x2, y2)

-- Empty : Invalid Move! --
checkMovement Empty _ _ _ = error "Invalid Move! @ checkMovement"
--
-- Check if path is clear, and Final destination is not same color --
checkPath :: Square -> [Square] -> Square -> Bool
checkPath Empty _ _ = error "Invalid Move! @ checkPath"
checkPath _         list  Empty    = all (== Empty) list
checkPath (White _) list (Black _) = all (== Empty) list
checkPath (Black _) list (White _) = all (== Empty) list
checkPath _ _ _ = False
