{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State (MonadState, StateT, runStateT, get, gets, modify, lift)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import qualified Data.Map as Map
import System.Console.ANSI (clearScreen, getTerminalSize, setCursorPosition, cursorDown, cursorBackward, cursorUp, setCursorColumn)
import Data.List (intersperse, intercalate)
import Data.Char (toLower)
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import Control.Monad (void, join, when)
----------------------------------------------------------------------------------------------------------------- |
-- | Data Types | --
data Piece    = Pawn | Knight | Bishop | Rook | Queen | King   deriving Eq
data Square   = White Piece   | Black Piece   | Empty          deriving Eq
data Player   = Player1       | Player2                        deriving Eq
type Position = (Int, Int)
type Board    = Map.Map Position Square
data Skipped  = None | Only (Position, Position) deriving Show
data Game = Game
    { getBoard             ::  Board
    , currentPlayer        ::  Player
    , opponent             ::  Player
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
    , playerBox            :: (Int,Int)
    , landscapeOrientation ::  Bool
    , whiteChar            ::  Char
    }
--
instance Show Piece where
    show :: Piece -> String
    show Pawn   = "Pawn"
    show Knight = "Knight"
    show Bishop = "Bishop"
    show Rook   = "Rook"
    show Queen  = "Queen"
    show King   = "King"
--
{- ♔ ♕ ♖ ♗ ♘ ♙ ♚ ♛ ♜ ♝ ♞ ♟ -}
instance Show Square where
    show :: Square -> String
    {- White -}
    show (White Pawn)   = "♟ "
    show (White Knight) = "♞ "
    show (White Bishop) = "♝ "
    show (White Rook)   = "♜ "
    show (White Queen)  = "♛ "
    show (White King)   = "♚ "
    {- Black -}
    show (Black Pawn)   = "♙ "
    show (Black Knight) = "♘ "
    show (Black Bishop) = "♗ "
    show (Black Rook)   = "♖ "
    show (Black Queen)  = "♕ "
    show (Black King)   = "♔ "
    show Empty     = ""
--
instance Show Player where
    show :: Player -> String
    show player = case player of
        Player1 -> "Player 1"
        Player2 -> "Player 2"
----------------------------------------------------------------------------------------------------------------- |
-- | Initializations | --
--
-- Graphics Configuration --
graphicsConfig :: ((Int,Int) , Char) -> GraphicsConfig
graphicsConfig ((h , w) , whiteChar) = GraphicsConfig (h, w) boardSize squareSize boardPosition playerBox landscapeOrientation whiteChar
    where
        thirdOfWidth           = div w 3
        isAlmostSquare         = h > thirdOfWidth && h < 2*thirdOfWidth
        landscapeOrientation   = h < div w 2
        s | landscapeOrientation = if isAlmostSquare
                        then h `div` 12
                        else h `div` 9
          | isAlmostSquare = w `div` 28
          | otherwise      = w `div` 23
    {- Square Size -}
        squareSize@(sh, sw) = (s , 2*s+1)
    {- Board Size -}
        boardSize @(bh, bw) = (sh*8 , sw*8)
    {- Board Position -}
        (vShift, hShift) | landscapeOrientation = (div (h - bh) 2 , div (w - bw) 2)
                         | otherwise            = (h - bh - 8     , div (w - bw) 2)
        boardPosition = (h - vShift, hShift)
    {- Player Box -}
        playerBoxWidth | landscapeOrientation = min 22 (hShift - 10)
                       | otherwise            = min 22 bw
        playerBoxSpace | landscapeOrientation = max 0 (playerBoxWidth - 10) `div` 4
                       | otherwise            = max 0 $ min 2 (div bw 15 - 2)
        playerBox = (5 + 2*playerBoxSpace , playerBoxSpace)
--
-- Starting Setup --
newBoard :: Board
newBoard = Map.mapWithKey f emptySquares where
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
newGame = Game newBoard Player1 Player2 [] [] Map.empty castlingInfo None
----------------------------------------------------------------------------------------------------------------- |
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
doPromotion :: (MonadState Game m , MonadIO m) => m ()
doPromotion = do
    {- Variables -}
    player  <- gets currentPlayer
    board   <- gets getBoard
    let  options       = [Queen, Rook, Bishop, Knight]
    let [pawnPosition] = case player of
            Player1 -> Map.keys $ Map.filterWithKey (\ (_, y) s -> y == 8 && s == White Pawn) board
            Player2 -> Map.keys $ Map.filterWithKey (\ (_, y) s -> y == 1 && s == Black Pawn) board
    {- IO actions -}
    write "Which piece do you want to promote your pawn to?\n\n"
    case player of
        Player1 -> liftIO $ mapM_ (\x -> putStrLn $ concat [" ", show (White x), " - ", show x]) options
        Player2 -> liftIO $ mapM_ (\x -> putStrLn $ concat [" ", show (Black x), " - ", show x]) options
    write "\n"
    piece <- getPromotion
    let updatedBoard = case player of
            Player1 -> Map.insert pawnPosition (White piece) board
            Player2 -> Map.insert pawnPosition (Black piece) board
    modify $ \ game -> game {getBoard = updatedBoard}
--
getPromotion :: MonadIO m => m Piece
getPromotion = do
    write " > "
    liftIO $  hFlush stdout
    answer <- liftIO getLine
    case map toLower answer of
        "queen"  -> return Queen
        "rook"   -> return Rook
        "bishop" -> return Bishop
        "knight" -> return Knight
        _ -> write "Invalid Response!\n" >> getPromotion
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
----------------------------------------------------------------------------------------------------------------- |
-- || Main || --
main :: IO ()
main = do
    config <- graphicsConfig <$> welcomeScreen
    runStateT (playGame config) newGame
    zeroCursor
--
{- Welcome Screen -}
welcomeScreen :: IO ((Int,Int) , Char)
welcomeScreen = do
    {- Variables -}
    let options        = [ '.' , '~' , ':' , '|' , '=' , '#' ]
    Just tSize         <- getTerminalSize
    let config         =  graphicsConfig (tSize, ' ')
    let (sh, sw)       =  squareSize      config
    let (bh, bw)       =  boardSize       config
    let (vPos, hPos)   =  boardPosition   config
    let y              =  vPos - bh + sh + 5
    let spacing        =  div (2*sw) 5
    let indent         =  div (mod (2*sw) 5) 2
    let numbers        =  concatMap (: replicate (sw - 1 + spacing) ' ') ['1' .. '6']
    let squares        =  zip [hPos + x * (sw + spacing) + indent | x <- [0..]] options
    let printSquare (x , c) = sequence_ [setCursorPosition (y + n) x >> putStr (replicate sw c) | n <- [0 .. sh-1]]
    {- IO actions -}
    clearScreen
    setCursorPosition (vPos - bh) (hPos + div bw 2)
    down 14 ; putStr "Welcome To The Game Of Chess"
    down 14
    down $ div bw 2 - indent
    cursorDown sh
    putStr "Please choose one of the following chessboard patterns below:"
    mapM_ printSquare squares
    setCursorPosition (y + sh + 1) (hPos + div sw 2 + indent)
    putStr numbers
    runReaderT resetCursor config
    putStr " > "
    i <- getChoice
    return (tSize , options !! i)
--
getChoice :: IO Int
getChoice = do
    hFlush stdout
    n <- readMaybe <$> getLine
    case n of
        Just x  -> if x > 0 && x < 7 then return $ x - 1
              else putStr errorMsg >> getChoice
        Nothing -> putStr errorMsg >> getChoice
    where errorMsg = "Invalid Input!\nPlease choose a number from 1 to 6 : "
--
{- playGame -}
playGame :: GraphicsConfig -> StateT Game IO ()
playGame config = do
    let renderBoard = join $ gets (\ game -> lift $ runReaderT (printBoard game) config)
    {- IO actions -}
    renderBoard
    playTurn
    renderBoard
    promotion <- checkPromotion
    when promotion (doPromotion >> renderBoard)
    changePlayer
    gameOver <- checkGameOver
    if gameOver
        then endGame  config
        else playGame config
--
endGame :: GraphicsConfig -> StateT Game IO ()
endGame config = do
    player  <- gets currentPlayer
    board   <- gets getBoard
    if kingNotInCheck player board
        then lift $ runReaderT  printStalemate         config
        else lift $ runReaderT (printCheckmate player) config
--
playTurn :: StateT Game IO ()
playTurn = do
    lift $ putStrLn "Which piece do you want to move?"
    pos1 <- takePieceToMove
    lift $ putStrLn "Where do you want to move it to?"
    pos2 <- takeDestination pos1
    updateSkippedSquare pos1 pos2
    replacedPiece <- makeMove pos1 pos2
    updateKilledPieces replacedPiece
--
takePieceToMove :: StateT Game IO Position
takePieceToMove = do
    {- Variables -}
    position      <- lift takeInput
    player        <- gets currentPlayer
    pieceToMove   <- gets $ (Map.! position) . getBoard
    let rightPlayer (White _) = player == Player1
        rightPlayer (Black _) = player == Player2
        rightPlayer  Empty    = False
    {- IO actions -}
    if  rightPlayer pieceToMove then do
        freeToMove <- possibleMovements position
        if freeToMove then return position
        else do
            lift $ putStrLn "No possible movements! Please choose another piece!"
            takePieceToMove
    else do
        lift $ putStrLn "Invalid Square!\nPlease choose from your own pieces!"
        takePieceToMove
--
takeDestination :: Position -> StateT Game IO Position
takeDestination pos1 = do
    {- Variables -}
    pos2          <- lift takeInput
    board         <- gets getBoard
    kingIsSafe    <- checkKingAfterMove pos1 pos2
    isCastling    <- checkCastleMove    pos1 pos2
    isEn_passant  <- checkEnPassantMove pos1 pos2
    let pieceToMove = board Map.! pos1
    {- IO actions -}
    if pos1 == pos2 then invalidMove 1
    else if checkMovement pieceToMove board pos1 pos2
        then if kingIsSafe   then return pos2 else invalidMove 2
        else if isEn_passant then prepareEnPassantMove >> return pos2
        else if isCastling   then return pos2
        else invalidMove 1
    where
        invalidMove n = do
            pieceToMove <- gets $ (Map.! pos1) . getBoard
            case n of
                1 -> lift . putStrLn   $ "Invalid Move! "
                2 -> lift . putStrLn   $ "This move leaves your king in check! "
                _ -> error "Not expected! @ invalidMove @ takeDestination"
            lift . putStrLn $ "Please choose another square to move\nyour piece " ++ show pieceToMove
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
        errorMessage1 = "* First character should be one of\n  the following alphabetical\n  letters: " ++ intersperse ' ' ['a'..'h']
        errorMessage2 = "* Second character should be\n  a number from 1 to 8"
--
-- | Print Board on terminal | --
--
printBoard :: Game -> ReaderT GraphicsConfig IO ()
printBoard game = do
    {- Variables -}
    let player     =  currentPlayer game
    let board      =  getBoard      game
    let positions  =  Map.keys board
    {- IO actions -}
    lift clearScreen
    printGrid         positions
    printPieces       player board
    printBorder       player
    printGameInfo     game
--
{- Grid -}
printGrid :: [Position] -> ReaderT GraphicsConfig IO ()
printGrid ks = do
    (sh, sw)      <- asks squareSize
    (vPos, hPos)  <- asks boardPosition
    let evenSquares = [(vPos - y*sh , hPos + (x-1)*sw) | (x, y) <- ks, odd y /= odd x]
    c <- asks whiteChar
    let printSquare (x, y) = sequence_ [setCursorPosition (x + n) y >> putStr (replicate sw c) | n <- [0 .. sh-1]]
    lift $ mapM_ printSquare evenSquares
    resetCursor
--
{- Pieces -}
printPieces :: Player -> Board -> ReaderT GraphicsConfig IO ()
printPieces player board = do
    (sh, sw)       <- asks squareSize
    (vPos, hPos)   <- asks boardPosition
    let printPiece (x, y) piece = case player of
            Player1 -> setCursorPosition (vPos -   y  *sh + div sh 2) (hPos + (x-1)*sw + div sw 2) >> print piece
            Player2 -> setCursorPosition (vPos - (9-y)*sh + div sh 2) (hPos + (8-x)*sw + div sw 2) >> print piece
    {- IO actions -}
    lift $ sequence_ . Map.mapWithKey printPiece $ board
    resetCursor
--
{- Border -}
printBorder :: Player -> ReaderT GraphicsConfig IO ()
printBorder player = do
    {- Variables -}
    (sh, sw)      <- asks squareSize
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    let hBorder = case player of
            Player1 -> concatMap (: replicate (sw-1) ' ')             ['a' .. 'h']
            Player2 -> concatMap (: replicate (sw-1) ' ') . reverse $ ['a' .. 'h']
    {- IO actions -}
    lift $ setCursorPosition (vPos + 1) (hPos + div sw 2)
    lift $ putStr hBorder
    lift $ setCursorPosition (vPos - bh - 2) (hPos + div sw 2)
    lift $ putStr hBorder
    let vBorder = case player of
            Player1 -> concatMap (: replicate (sh-1) ' ')          ['1' .. '8']  `zip` [0..]
            Player2 -> concatMap (: replicate (sh-1) ' ') (reverse ['1' .. '8']) `zip` [0..]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos - 2)      >> putStr [c] | (c, i) <- vBorder]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos + 1 + bw) >> putStr [c] | (c, i) <- vBorder]
    resetCursor
--
{- Game info -}
printGameInfo :: Game -> ReaderT GraphicsConfig IO ()
printGameInfo game = do
    {- Variables -}
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    isLandscape   <- asks landscapeOrientation
    {- IO actions -}
    if isLandscape then do
        lift $ setCursorPosition  (vPos - bh    )  (hPos      - div hPos 2 - 1)
        printPlayerBoxInLandscape game Player1
        lift $ setCursorPosition  (vPos - bh    )  (hPos + bw + div hPos 2 + 1)
        printPlayerBoxInLandscape game Player2
    else do
        lift $ setCursorPosition  (vPos - bh - 5)  (hPos      + div bw   2    )
        printPlayerBoxInPortrait False game (opponent game)
        lift $ setCursorPosition  (vPos      + 3)  (hPos      + div bw   2    )
        printPlayerBoxInPortrait True  game (currentPlayer game)
    resetCursor
--
printPlayerBoxInLandscape :: Game -> Player -> ReaderT GraphicsConfig IO ()
printPlayerBoxInLandscape game player = do
    {- Variables -}
    (dx, _) <- asks playerBox
    let markSquares        = zip $ intercalate [True] [replicate 4 False | _ <- [1..3]]
    let printKilledPieces  = mapM_ printPiece . markSquares . (++ repeat Empty)
    {- IO actions -}
    down 4
    write . show $ player
    down 9 ; write "----------"
    down 5 >> down dx
    case player of
        Player1 -> printKilledPieces . killedBlackPieces $ game
        Player2 -> printKilledPieces . killedWhitePieces $ game
    down (dx - 2)
    printMessageBox game player
    resetCursor
--
printPlayerBoxInPortrait :: Bool -> Game -> Player -> ReaderT GraphicsConfig IO ()
printPlayerBoxInPortrait turn game player = do
    {- Variables -}
    (_, hPos)       <- asks boardPosition
    (_, bw)         <- asks boardSize
    (_, space)      <- asks playerBox
    let killedPieces = case player of
            Player1 -> killedBlackPieces game
            Player2 -> killedWhitePieces game
    let printKilledPieces = mapM_ (printPiece . (,) False) . take 15 . (++ repeat Empty)
    {- IO actions -}
    down 4
    if turn
        then do
            up 0   ; write . show $ player
            down 9 ; write "----------"
            down (7 * space + 20)
            printKilledPieces killedPieces
            lift $ setCursorColumn (hPos + div bw 2)
            printMessageBox game player
        else do
            write . show $ player
            up 9  ; write "----------"
            up (7 * space + 20)
            printKilledPieces killedPieces
            up 0  ; lift  $ setCursorColumn hPos
            up 0  ; write $ replicate bw '_'
    resetCursor
--
printMessageBox :: Game -> Player -> ReaderT GraphicsConfig IO ()
printMessageBox game playerMsgBox = do
    (dx, _) <- asks playerBox -- Half of message box width
    {- Variables -}
    let playerInGame  = currentPlayer  game
    let board         = getBoard       game
    let drawLine      = write $ replicate (dx*2) '_'
    {- IO actions -}
    down dx ; drawLine
    down dx
    down 5
    if playerMsgBox == playerInGame
        then write "Your Turn!"
        else blank 10
    down 12
    down 0
    if kingNotInCheck playerMsgBox board
        then blank 14
        else write "King in Check!"
    down 7
    down dx ; drawLine
    resetCursor
--
printPiece :: (Bool , Square) -> ReaderT GraphicsConfig IO ()
printPiece (isNewLine , square) = do
    (dx, space) <- asks playerBox
    let printSquare = lift . putStr $ show square
    if isNewLine then if square /= Empty
        then printSquare >> down dx >> down dx
        else blank 2     >> down dx >> down dx
    else if square /= Empty
        then printSquare >> blank space
        else blank (2 + space)
--
{- Game Over -}
printStalemate :: ReaderT GraphicsConfig IO ()
printStalemate = do
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    let n = 22
    lift   $ setCursorPosition (vPos - 5 - div bh 2) (hPos - 11 + div bw 2)
    write  $ replicate n '*'
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
printCheckmate :: Player -> ReaderT GraphicsConfig IO ()
printCheckmate player = do
    (bh, bw)      <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    let n = 22
    lift   $ setCursorPosition (vPos - 5 - div bh 2) (hPos - 11 + div bw 2)
    write  $ replicate n '*'
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
-- | Tools | --
--
up :: MonadIO m => Int -> m ()
up n = do
    liftIO $ cursorUp 1
    liftIO $ cursorBackward n
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
resetCursor :: ReaderT GraphicsConfig IO ()
resetCursor = do
    isLandscape   <- asks landscapeOrientation
    (bh, _)       <- asks boardSize
    (vPos, hPos)  <- asks boardPosition
    if isLandscape then if hPos > 35
        then lift $ setCursorPosition (vPos - bh + 20 )   0
        else lift $ setCursorPosition (vPos      +  2 )   0
        else lift $ setCursorPosition (vPos      + 14 )   0
--
zeroCursor :: IO ()
zeroCursor = do
    Just (h , _) <- getTerminalSize
    setCursorPosition (h - 1) 0
----------------------------------------------------------------------------------------------------------------- |
-- End of Code -------------------------------------------------------------------------------------------------- |
----------------------------------------------------------------------------------------------------------------- |