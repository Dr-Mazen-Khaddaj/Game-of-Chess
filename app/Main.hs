{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State (lift, put, get, modify, State, runState, StateT, runStateT, gets, MonadState)
import Control.Monad.Reader (ReaderT, runReaderT, asks, MonadReader)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.Map as Map
import System.Console.ANSI (clearScreen, getTerminalSize, setCursorPosition, cursorDown, cursorBackward)
import Data.List (intersperse, intercalate, groupBy)
import Data.Char (chr, ord)
import Data.Function (on)
----------------------------------------------------------------------------------------------------------------- |
-- | Data Types | --
data Piece    = Pawn | Knight | Bishop | Rook | Queen | King   deriving Eq
data Square   = White Piece   | Black Piece   | Empty          deriving Eq
data Player   = Player1       | Player2                        deriving Eq
type Position = (Int, Int)
data Skipped  = None | Only (Position, Position) deriving Show
newtype Board = Board
    { getSquares           ::  Map.Map Position Square
    }
data Game = Game
    { getBoard             ::  Board
    , currentPlayer        ::  Player
    , killedWhitePieces    :: [Square]
    , killedBlackPieces    :: [Square]
    , movedPieces          ::  Map.Map Position Square
    , getCastlingInfo      ::  Map.Map (Position, Position) CastlingInfo
    , skippedSquare        ::  Skipped
    }
data CastlingInfo = CastlingInfo
    { getRookPosition      ::  Position
    , getRookDestination   ::  Position
    , getRookPath          :: [Position]
    , getKingPath          :: [Position]
    }
data GraphicsConfig = GraphicsConfig
    { terminalSize         :: (Int,Int)
    , boardSize            :: (Int,Int)
    , squareSize           :: (Int,Int)
    , boardPosition        :: (Int,Int)
    , landscapeOrientation ::  Bool
    , whiteChar            ::  Char
    }
--
instance Show Piece where
    show :: Piece -> String
    show Pawn   = "♟"
    show Knight = "♞"
    show Bishop = "♝"
    show Rook   = "♜"
    show Queen  = "♛"
    show King   = "♚"
--
instance Show Square where
    show :: Square -> String
    show (White p) = show p ++ " "
    show (Black p) = (++ " ") . map (chr . (\ n -> n - 6 ) . ord) $ show p
    show Empty     = ""
--
instance Show Board where
    show :: Board -> String
    show (Board list) = concat $ Map.mapWithKey (\ pos sqr -> show pos ++ "\t : " ++ show sqr ++ "\t\t ") list
----------------------------------------------------------------------------------------------------------------- |
-- | Initializations | --
--
-- Graphics Configuration --
graphicsConfig :: IO GraphicsConfig
graphicsConfig = do
    Just (h, w) <- getTerminalSize
    let landscapeOrientation = h < div w 2
    let s = min h (div w 2) `div` 9
    let squareSize@(sh, sw) = (s , 2*s+1)
    let boardSize @(bh, bw) = (sh*8 , sw*8)
    let (vShift, hShift) | landscapeOrientation = (div (h - bh) 2 , div (w - bw) 2)
                         | otherwise            = (div (h - bh) 2 , div (w - bw) 2)
    let boardPosition = (h - vShift, hShift)
    let whiteChar = ':'
    return $ GraphicsConfig (h, w) boardSize squareSize boardPosition landscapeOrientation whiteChar
--
-- Starting Setup --
newBoard :: Board
newBoard = Board $ Map.mapWithKey f emptySquares where
    emptySquares = Map.fromList $ zip [ (x,y) | x <- [1..8], y <- [1..8]] (repeat Empty)
    f k@(_, y) _ | k == (1,1) || k == (8,1) = White Rook
                 | k == (2,1) || k == (7,1) = White Knight
                 | k == (3,1) || k == (6,1) = White Bishop
                 | k == (4,1)               = White Queen
                 | k == (5,1)               = White King
                 | y == 2                   = White Pawn
                 | k == (1,8) || k == (8,8) = Black Rook
                 | k == (2,8) || k == (7,8) = Black Knight
                 | k == (3,8) || k == (6,8) = Black Bishop
                 | k == (4,8)               = Black Queen
                 | k == (5,8)               = Black King
                 | y == 7                   = Black Pawn
                 | otherwise = Empty
--
-- Castling Info --
castlingInfo :: Map.Map (Position,Position) CastlingInfo
castlingInfo =
    Map.insert blackHighMove bHCinfo $
    Map.insert blackLowMove  bLCinfo $
    Map.insert whiteHighMove wHCinfo $
    Map.insert whiteLowMove  wLCinfo Map.empty
    where
        whiteLowMove  = ((5,1), (3,1))
        whiteHighMove = ((5,1), (7,1))
        blackLowMove  = ((5,8), (3,8))
        blackHighMove = ((5,8), (7,8))
        wLCinfo = CastlingInfo
            { getRookPosition    = (1, 1)
            , getRookDestination = (4, 1)
            , getRookPath        = [(2, 1), (3, 1), (4, 1)]
            , getKingPath        = [(5, 1), (4, 1), (3, 1)]
            }
        wHCinfo = CastlingInfo
            { getRookPosition    = (8, 1)
            , getRookDestination = (6, 1)
            , getRookPath        = [(7, 1), (6, 1)]
            , getKingPath        = [(5, 1), (6, 1), (7, 1)]
            }
        bLCinfo = CastlingInfo
            { getRookPosition    = (1, 8)
            , getRookDestination = (4, 8)
            , getRookPath        = [(2, 8), (3, 8), (4, 8)]
            , getKingPath        = [(3, 8), (4, 8), (5, 8)]
            }
        bHCinfo = CastlingInfo
            { getRookPosition    = (8, 8)
            , getRookDestination = (6, 8)
            , getRookPath        = [(7, 8), (6, 8)]
            , getKingPath        = [(5, 8), (6, 8), (7, 8)]
            }
--
-- Initialize Game --
newGame :: Game
newGame = Game newBoard Player1 [] [] Map.empty castlingInfo None
----------------------------------------------------------------------------------------------------------------- |
-- | Functions | --
--
-- Done
makeMove :: MonadState Game m => Position -> Position -> m Square
makeMove position destination = do
    squares <- gets $ getSquares . getBoard
    let pieceToMove      = squares Map.! position
    let pieceToReplace   = squares Map.! destination
    let updatedBoard     = Board . Map.insert destination pieceToMove    . Map.insert position Empty       $ squares
    newMovedPieces      <- gets  $ Map.insert destination pieceToReplace . Map.insert position pieceToMove . movedPieces
    modify (\ game -> game { movedPieces  =  newMovedPieces
                           , getBoard     =  updatedBoard
                            }
            )
    return pieceToReplace
--
updateSkippedSquare :: MonadState Game m => Position -> Position -> m ()
updateSkippedSquare position destination = do
    player       <- gets currentPlayer
    pieceToMove  <- gets $ (Map.! position) . getSquares . getBoard
    let isPawn   =  pieceToMove `elem` [White Pawn, Black Pawn]
    let deltaY   =  abs $ snd position - snd destination
    let stepBack =  case player of Player1 -> (+) (-1) ; Player2 -> (+) 1
    if isPawn && deltaY == 2
        then modify (\ game -> game {skippedSquare = Only (fmap stepBack destination, destination)})
        else modify (\ game -> game {skippedSquare = None})
--
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
checkKingAfterMove :: MonadState Game m => Position -> Position -> m Bool
checkKingAfterMove position destination = do
    player  <- gets currentPlayer
    squares <- gets $ getSquares . getBoard
    let pieceToMove = squares Map.! position
    let doMove = Board . Map.insert destination pieceToMove . Map.insert position Empty
    pure . kingNotInCheck player . doMove $ squares
--
-- Takes the position of a piece and returns all possible moves of that piece -- Done
possibleMovements :: MonadState Game m => Position -> m Bool
possibleMovements position = do
    board <- gets getBoard
    let squares        = getSquares board
    let pieceToMove    = squares Map.! position
    let checkMove k _  = k /= position && checkMovement pieceToMove board position k
    let checkKing k _  = checkKingAfterMove position k
    let enPassant k _  = checkEnPassantMove position k
    let possibleMoves  = Map.filterWithKey checkMove squares
    foundEnPassantMove <- (||) . or <$> sequence (Map.mapWithKey enPassant squares)
    foundEnPassantMove . or <$> sequence (Map.mapWithKey checkKing possibleMoves)
--
-- Takes a board, and a final position (destination), and returns all possible moves to that destination --
possibleDestination :: Board -> Position -> Map.Map Position Square
possibleDestination board position = Map.filterWithKey validMove . getSquares $ board
    where
        validMove k pieceToMove = k /= position && pieceToMove /= Empty && checkMovement pieceToMove board k position
--
-- Check King --
kingNotInCheck :: Player -> Board -> Bool
kingNotInCheck player board@(Board squares) = Map.null $ possibleDestination board kingPosition
    where kingPosition = case player of
            Player1 -> head $ Map.keys $ Map.filter (== White King) squares
            Player2 -> head $ Map.keys $ Map.filter (== Black King) squares
--
-- Check Castling Move --
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
            squares         <- gets $ getSquares . getBoard
            let clearPath   = all ((== Empty) . (squares Map.!)) rookPath
            kingPathNotInCheck <- mapM (checkKingAfterMove kingPosition) kingPath
            didNotMoveKing <- gets $ Map.notMember kingPosition . movedPieces
            didNotMoveRook <- gets $ Map.notMember rookPosition . movedPieces
            if and (didNotMoveKing : didNotMoveRook : clearPath : kingPathNotInCheck)
                then makeMove rookPosition rookDestination >> return True
                else return False
-- Done
checkEnPassantMove :: MonadState Game m => Position -> Position -> m Bool
checkEnPassantMove position destination = do
    squares          <- gets $ getSquares . getBoard
    skippedSquare    <- gets skippedSquare
    let pieceToMove  =  squares Map.! position
    let isPawn       =  pieceToMove `elem` [White Pawn, Black Pawn]
    case skippedSquare of
        Only (posEmpty, posPawn) -> if isPawn && destination == posEmpty
            then validateEnPassantMove posPawn position destination
            else return False
        None  -> return False
-- Done
validateEnPassantMove :: MonadState Game m => Position -> Position -> Position -> m Bool
validateEnPassantMove posPawn position destination = do
    player   <- gets currentPlayer
    squares  <- gets $ getSquares . getBoard
    let enemyPiece     = squares Map.! posPawn
    let pieceToMove    = squares Map.! position
    let modifiedBoard  = Board . Map.insert destination enemyPiece  . Map.insert posPawn Empty $ squares
    let finalBoard     = Board . Map.insert destination pieceToMove . Map.insert posPawn Empty . Map.insert position Empty $ squares
    let checkMove      = checkMovement pieceToMove modifiedBoard position destination
    let checkKing      = kingNotInCheck player finalBoard
    return $ checkMove && checkKing
--
prepareEnPassantMove :: MonadState Game m => m ()
prepareEnPassantMove = do
    skippedSquare <- gets skippedSquare
    case skippedSquare of
        Only (posEmpty, posPawn) -> makeMove posPawn posEmpty >> return ()
        None -> error "Can't prepare En passant move!"
-- Do not use delete !!
-- consider changing Board from newType to Type
checkGameOver :: MonadState Game m => m Bool
checkGameOver = do
    player   <- gets currentPlayer
    squares  <- gets $ getSquares . getBoard
    let player'sPiece  (White _)  =  player == Player1
        player'sPiece  (Black _)  =  player == Player2
        player'sPiece   Empty     =  False
    let positions = Map.keys . Map.filter player'sPiece $ squares
    not . or <$> mapM possibleMovements positions
----------------------------------------------------------------------------------------------------------------- |
-- | Checkings | --
--
-- Check Move --
checkMovement :: Square -> Board -> Position -> Position -> Bool
-- Pawn --
checkMovement (White Pawn) board (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2-1           = finalDestination == Empty
    | x1 == x2 && y2 == 4              = path == [Empty, Empty]
    | abs (x1 - x2) == 1 && y1 == y2-1 = case finalDestination of Black _ -> True; _ -> False
    | otherwise = False
    where
        path@(finalDestination:_) = [getSquares board Map.! (x2, y2-n) | n <- [0,1]]

checkMovement (Black Pawn) board (x1, y1) (x2, y2)
    | x1 == x2 && y1 == y2+1           = finalDestination == Empty
    | x1 == x2 && y2 == 5              = path == [Empty, Empty]
    | abs (x1 - x2) == 1 && y1 == y2+1 = case finalDestination of White _ -> True; _ -> False
    | otherwise = False
    where
        path@(finalDestination:_) = [getSquares board Map.! (x2, y2+n) | n <- [0,1]]

-- Rook --
checkMovement (White Rook) board (x1, y1) (x2, y2)
    | x1 == x2 = checkPath (White Rook) pathX finalDestination
    | y1 == y2 = checkPath (White Rook) pathY finalDestination
    | otherwise = False
    where
        finalDestination = getSquares board Map.! (x2 , y2)
        pathX = [getSquares board Map.! (x2 , y2 - n * signum (y2 - y1)) | n <- [1 .. abs (y2 - y1) - 1]]
        pathY = [getSquares board Map.! (x2 - n * signum (x2 - x1) , y2) | n <- [1 .. abs (x2 - x1) - 1]]

checkMovement (Black Rook) board (x1, y1) (x2, y2)
    | x1 == x2 = checkPath (Black Rook) pathX finalDestination
    | y1 == y2 = checkPath (Black Rook) pathY finalDestination
    | otherwise = False
    where
        finalDestination = getSquares board Map.! (x2 , y2)
        pathX = [getSquares board Map.! (x2 , y2 - n * signum (y2 - y1)) | n <- [1 .. abs (y2 - y1) - 1]]
        pathY = [getSquares board Map.! (x2 - n * signum (x2 - x1) , y2) | n <- [1 .. abs (x2 - x1) - 1]]

-- Bishop --
checkMovement (White Bishop) board (x1, y1) (x2, y2)
    | abs (y2 - y1) == abs (x2 - x1) = checkPath (White Bishop) pathX finalDestination
    | otherwise = False
    where
        (finalDestination : pathX) = [getSquares board Map.! (x2 - n * signum (x2 - x1) , y2 - n * signum (y2 - y1)) | n <- [0 .. abs (y2 - y1) - 1]]

checkMovement (Black Bishop) board (x1, y1) (x2, y2)
    | abs (y2 - y1) == abs (x2 - x1) = checkPath (Black Bishop) pathX finalDestination
    | otherwise = False
    where
        (finalDestination : pathX) = [getSquares board Map.! (x2 - n * signum (x2 - x1) , y2 - n * signum (y2 - y1)) | n <- [0 .. abs (y2 - y1) - 1]]

-- Knight --
checkMovement (White Knight) board (x1, y1) (x2, y2)
    | abs (x2 - x1) == 1 && abs (y2 - y1) == 2 = checkPath (White Knight) [] finalDestination
    | abs (x2 - x1) == 2 && abs (y2 - y1) == 1 = checkPath (White Knight) [] finalDestination
    | otherwise = False
    where
        finalDestination = getSquares board Map.! (x2, y2)

checkMovement (Black Knight) board (x1, y1) (x2, y2)
    | abs (x2 - x1) == 1 && abs (y2 - y1) == 2 = checkPath (Black Knight) [] finalDestination
    | abs (x2 - x1) == 2 && abs (y2 - y1) == 1 = checkPath (Black Knight) [] finalDestination
    | otherwise = False
    where
        finalDestination = getSquares board Map.! (x2, y2)

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
        finalDestination = getSquares board Map.! (x2, y2)

checkMovement (Black King) board (x1, y1) (x2, y2)
    | deltaPos `elem` [(1,0), (0,1), (1,1)] = checkPath (Black King) [] finalDestination
    | otherwise = False
    where
        deltaPos = (abs (x2 - x1) , abs (y2 - y1))
        finalDestination = getSquares board Map.! (x2, y2)

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
----------------------------------------------------------------------------------------------------------------- |
-- Promotion
-- || Main || --
main :: IO ()
main = do
    (v, board) <- runStateT playGame newGame
    return ()
-- remove first `printBoard`
playGame :: StateT Game IO ()
playGame = do
    printBoard
    -- Testing ..
    config  <- lift graphicsConfig
    runReaderT printStalemate config
    --
    playTurn
    printBoard
    changePlayer
    gameOver <- checkGameOver
    if gameOver
        then endGame
        else playGame
--
endGame :: StateT Game IO ()
endGame = do
    player  <- gets currentPlayer
    board   <- gets getBoard
    config  <- lift graphicsConfig
    if kingNotInCheck player board
        then runReaderT printStalemate config
        else runReaderT printCheckmate config
--
changePlayer :: StateT Game IO ()
changePlayer = do
    player <- gets currentPlayer
    case player of
        Player1 -> modify (\ game -> game {currentPlayer = Player2})
        Player2 -> modify (\ game -> game {currentPlayer = Player1})
--
-- In main: Welcome screen, gameOver screen
-- Do separate DataType for all questions/IO output
playTurn :: StateT Game IO ()
playTurn = do
    lift $ putStrLn "Which piece do you want to move?"
    pos1 <- takePieceToMove
    lift $ putStrLn "Where do you want to move it to?"
    pos2 <- takeDestination pos1
    updateSkippedSquare pos1 pos2
    replacedPiece <- makeMove pos1 pos2
    updateKilledPieces replacedPiece
-- Good to add "King in Check"
takePieceToMove :: StateT Game IO Position
takePieceToMove = do
    position      <- lift takeInput
    player        <- gets currentPlayer
    pieceToMove   <- gets $ (Map.! position) . getSquares . getBoard
    let rightPlayer (White _) = player == Player1
        rightPlayer (Black _) = player == Player2
        rightPlayer  Empty    = False
    if  rightPlayer pieceToMove then do
        freeToMove <- possibleMovements position
        if freeToMove then return position
        else do
            lift $ putStrLn "No possible movements! Please choose another piece!"
            takePieceToMove
    else do
        lift $ putStrLn "Invalid Square! Please choose from your own pieces!"
        takePieceToMove
-- Do seperate function that handles all errors!
takeDestination :: Position -> StateT Game IO Position
takeDestination pos1 = do
    pos2          <- lift takeInput
    board         <- gets getBoard
    kingIsSafe    <- checkKingAfterMove pos1 pos2
    isCastling    <- checkCastleMove    pos1 pos2
    isEn_passant  <- checkEnPassantMove pos1 pos2
    let pieceToMove = getSquares board Map.! pos1
    if pos1 == pos2 then invalidMove 1
    else if checkMovement pieceToMove board pos1 pos2
        then if kingIsSafe   then return pos2 else invalidMove 2
        else if isEn_passant then prepareEnPassantMove >> return pos2
        else if isCastling   then return pos2
        else invalidMove 1
    where
        invalidMove n = do
            pieceToMove <- gets $ (Map.! pos1) . getSquares . getBoard
            case n of
                1 -> lift . putStr   $ "Invalid Move! "
                2 -> lift . putStr   $ "This move leaves your king in check! "
                _ -> error "Not expected! @ invalidMove @ takeDestination"
            lift . putStrLn $ "Please choose another square to move your piece " ++ show pieceToMove
            takeDestination pos1
--
takeInput :: IO Position
takeInput = do
    input <- fmap processInput getLine
    case input of
        Left error -> do
            putStrLn error
            takeInput
        Right position -> return position
    where
        processInput [c, i] = case lookup c (zip ['a'..'h'] [1..]) of
            Just x -> case lookup i (zip ['1'..'8'] [1..]) of
                Just y  -> return (x, y)
                Nothing -> Left errorMessage2
            Nothing -> Left errorMessage1
        processInput _ = Left $ intercalate "\n" ["Invalid Input!", errorMessage1, errorMessage2]
        errorMessage1 = "First character should be one of the following alphabetical letters: " ++ intersperse ' ' ['a'..'h']
        errorMessage2 = "Second character should be a number from 1 to 8"
--
-- | Print Board on terminal | --
--
printBoard :: StateT Game IO ()
printBoard = do
    lift clearScreen
    board  <- gets getBoard
    config <- lift graphicsConfig
    player <- gets currentPlayer
    lift $ runReaderT (printGrid . Map.keys . getSquares $ board) config
    case player of
        Player1 -> do
            lift $ runReaderT (printPieces          board) config
            lift $ runReaderT  printBorder                 config
        Player2 -> do
            lift $ runReaderT (printPiecesInReverse board) config
            lift $ runReaderT  printBorderInReverse        config
    game <- get
    lift $ runReaderT (printGameInfo game) config
--
printBorder :: ReaderT GraphicsConfig IO ()
printBorder = do
    (sh, sw)      <- asks squareSize
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    let hBorder = concatMap (: replicate (sw-1) ' ') ['a' .. 'h']
    lift $ setCursorPosition (vPos + 1) (hPos + div sw 2)
    lift $ putStr hBorder
    lift $ setCursorPosition (vPos - bh - 2) (hPos + div sw 2)
    lift $ putStr hBorder
    let vBorder = concatMap (: replicate (sh-1) ' ') ['1' .. '8'] `zip` [0..]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos - 2)      >> putStr [c] | (c, i) <- vBorder]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos + 1 + bw) >> putStr [c] | (c, i) <- vBorder]
--
-- Print pieces from Player 2 perspective
printBorderInReverse :: ReaderT GraphicsConfig IO ()
printBorderInReverse = do
    (sh, sw)      <- asks squareSize
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    let hBorder = concatMap (: replicate (sw-1) ' ') . reverse $ ['a' .. 'h']
    lift $ setCursorPosition (vPos + 1) (hPos + div sw 2)
    lift $ putStr hBorder
    lift $ setCursorPosition (vPos - bh - 2) (hPos + div sw 2)
    lift $ putStr hBorder
    let vBorder = concatMap (: replicate (sh-1) ' ') (reverse ['1' .. '8']) `zip` [0..]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos - 2)      >> putStr [c] | (c, i) <- vBorder]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos + 1 + bw) >> putStr [c] | (c, i) <- vBorder]
--
printGrid :: [Position] -> ReaderT GraphicsConfig IO ()
printGrid ks = do
    (sh, sw)      <- asks squareSize
    (vPos, hPos)  <- asks boardPosition
    let evenSquares = [(vPos - y*sh , hPos + (x-1)*sw) | (x, y) <- ks, odd y /= odd x]
    c <- asks whiteChar
    let printSquare (x, y) = sequence_ [setCursorPosition (x + n) y >> putStr (replicate sw c) | n <- [0 .. sh-1]]
    lift $ mapM_ printSquare evenSquares
--
printPieces :: Board -> ReaderT GraphicsConfig IO ()
printPieces (Board squares) = do
    (sh, sw)      <- asks squareSize
    (vPos, hPos)  <- asks boardPosition
    let printPiece (x, y) piece = setCursorPosition (vPos - y*sh + div sh 2) (hPos + (x-1)*sw + div sw 2) >> print piece
    lift $ sequence_ . Map.mapWithKey printPiece $ squares
--
-- Print pieces from Player 2 perspective
printPiecesInReverse :: Board -> ReaderT GraphicsConfig IO ()
printPiecesInReverse (Board squares) = do
    (sh, sw)      <- asks squareSize
    (vPos, hPos)  <- asks boardPosition
    let printPiece (x, y) piece = setCursorPosition (vPos - (9-y)*sh + div sh 2) (hPos + (8-x)*sw + div sw 2) >> print piece
    lift $ sequence_ . Map.mapWithKey printPiece $ squares
-- Need optimization + generalisation + more work
printGameInfo :: Game -> ReaderT GraphicsConfig IO ()
printGameInfo game = do
    (bh, bw)     <- asks boardSize
    (vPos, hPos) <- asks boardPosition
    isLandscape  <- asks landscapeOrientation
    if isLandscape
        then lift $ setCursorPosition (vPos - bh) (div hPos 2 - 5)
        else lift $ setCursorPosition (vPos - bh - 5) (hPos + div bw 2 - 5)
    write "Player 1"
    down 9 ; write "----------      "
    lift $ mapM_ print5killedPieces . groupBy5 . killedBlackPieces $ game
    if isLandscape
        then lift $ setCursorPosition (vPos - bh) (hPos + bw + div hPos 2 - 5)
        else lift $ setCursorPosition (vPos + 3) (hPos + div bw 2 - 5)
    write "Player 2"
    down 9 ; write "----------      "
    lift $ mapM_ print5killedPieces . groupBy5 . killedWhitePieces $ game
    lift $ setCursorPosition (vPos-10) 0
    where
        groupBy5 = groupBy (on (==) fst) . zip (concatMap (replicate 5) [1..4])
        print5killedPieces groupOfsquares = do
            down 11 ; down 11
            sequence_ . intersperse (putStr "   ") . map (putStr . show . snd) $ groupOfsquares
--
printStalemate :: MonadIO m => ReaderT GraphicsConfig m ()
printStalemate = do
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    let n = 22
    liftIO $ setCursorPosition (vPos - 5 - div bh 2) (hPos - 11 + div bw 2)
    write $ replicate n '*'
    down n ; blank n
    down n ; blank n
    down n ; blank n
    down n ; write "      Stalemate!      "
    down n ; blank n
    down n ; blank n
    down n ; blank n
    down n ; blank n
    down n ; write $ replicate n '*'
    resetCursor
--
printCheckmate :: (MonadState Game m , MonadIO m) => ReaderT GraphicsConfig m ()
printCheckmate = do
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    player        <- gets currentPlayer
    let n = 22
    liftIO $ setCursorPosition (vPos - 5 - div bh 2) (hPos - 11 + div bw 2)
    write $ replicate n '*'
    down n ; blank n
    down n ; blank n
    down n ; write "      Checkmate!      "
    down n ; blank n
    down n ; blank n
    down n
    case player of
        Player1 -> write "    Player 2 Wins!    "
        Player2 -> write "    Player 1 Wins!    "
    down n ; blank n
    down n ; blank n
    down n ; write $ replicate n '*'
    resetCursor
--
down :: MonadIO m => Int -> m ()
down n = do
    liftIO $ cursorDown 1
    liftIO $ cursorBackward n
--
blank :: MonadIO m => Int -> m ()
blank n = liftIO $ putStr $ replicate n ' '
--
write :: MonadIO m => String -> m ()
write text = liftIO $ putStr text
--
resetCursor :: (MonadReader GraphicsConfig m , MonadIO m) => m ()
resetCursor = do
    (h, w) <- asks terminalSize
    liftIO $ setCursorPosition h w
----------------------------------------------------------------------------------------------------------------- |