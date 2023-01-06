{-# LANGUAGE InstanceSigs #-}
import Control.Monad.State (lift, put, get, modify, State, runState, StateT, runStateT, gets)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad (forever)
import qualified Data.Map as Map
import System.Console.ANSI (clearScreen, getTerminalSize, setCursorPosition)
import Data.List (intersperse, intercalate)
import Data.Char (chr, ord)
----------------------------------------------------------------------------------------------------------------- |
-- | Data Types | --
data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving Eq
data Square = White Piece | Black Piece | Empty deriving Eq
type Position = (Int, Int)
newtype Board = Board { getSquares :: Map.Map Position Square }
data Player = Player1 | Player2 deriving Eq
data Game = Game
    { getBoard :: Board
    , getTest :: Int
    }
data GraphicsConfig = GraphicsConfig
    { terminalSize  :: (Int,Int)
    , boardSize     :: (Int,Int)
    , squareSize    :: (Int,Int)
    , boardPosition :: (Int,Int)
    , landscapeOrientation :: Bool
    , whiteChar :: Char
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
--
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
                         | otherwise            = (div w 4 - div h 2 , 0)
    let boardPosition = (h - vShift, hShift)
    let whiteChar = '|'
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

-- Initialize Game --
newGame :: Game
newGame = Game newBoard 0
--
-- | Functions | --
--
-- Make a Move --
makeMove :: Position -> Position -> State Board Square
makeMove initialPosition finalPosition = do
    board <- get
    let pieceToMove    = getSquares board Map.! initialPosition
    let pieceToReplace = getSquares board Map.! finalPosition
    modify (Board . Map.insert initialPosition Empty       . getSquares)
    modify (Board . Map.insert finalPosition   pieceToMove . getSquares)
    return pieceToReplace
--
-- | Checkings | --
-- Check Move --------------------------------------------------------------------------------------------------- |
-- !!! -  en passant, Castling !!! --
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
    | y2 - y1 == x2 - x1 = checkPath (Black Bishop) pathX finalDestination
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

-- Check if path is clear, and Final destination is not same color --
checkPath :: Square -> [Square] -> Square -> Bool
checkPath Empty _ _ = error "Invalid Move! @ checkPath"
checkPath _         list  Empty    = all (== Empty) list
checkPath (White _) list (Black _) = all (== Empty) list
checkPath (Black _) list (White _) = all (== Empty) list
checkPath _ _ _ = False
--
-- Takes a board and the initial position of a piece and returns all possible moves of that piece --
possibleMovements :: Board -> Position -> Map.Map Position Square
possibleMovements board position = Map.filterWithKey validMove . Map.delete position . getSquares $ board
    where
        validMove k _ = checkMovement pieceToMove board position k
        pieceToMove = getSquares board Map.! position
--
-- Takes a board, and a final position (destination), and returns all possible moves to that destination --
possibleDestination :: Board -> Position -> Map.Map Position Square
possibleDestination board position = Map.filterWithKey validMove . Map.delete position . getSquares $ board
    where
        validMove k pieceToMove = checkMovement pieceToMove board k position
--
----------------------------------------------------------------------------------------------------------------- |
-- || Main || --
main :: IO ()
main = do
    (v, board) <- runStateT playGame newGame
    return ()
--
playGame :: StateT Game IO ()
playGame = forever $ do
    printBoard
    lift $ putStrLn "Player 1"
    playTurn Player1
    lift $ putStrLn "Player 2"
    playTurn Player2
--
playTurn :: Player -> StateT Game IO ()
playTurn player = do
    lift $ putStrLn "What piece do you want to Move?"
    pos1 <- takePieceToMove player
    lift $ putStrLn "Where do you want to move it to?"
    pos2 <- takeDestination pos1
    board <- gets getBoard
    (replacedPiece, updatedBoard) <- pure $ runState (makeMove pos1 pos2) board
    modify (\ game -> game {getBoard = updatedBoard})
    printBoard
    -- Do more things
--
takePieceToMove :: Player -> StateT Game IO Position
takePieceToMove player = do
    position <- lift takeInput
    board    <- gets getBoard
    let pieceToMove = getSquares board Map.! position
    if rightPlayer pieceToMove
        then if not $ Map.null $ possibleMovements board position
            then return position
            else do
                lift $ putStrLn "No possible movements! Please choose another piece!"
                takePieceToMove player
        else do
            lift $ putStrLn "Invalid Square! Please choose from your own pieces!"
            takePieceToMove player
    where
        rightPlayer pieceToMove = case pieceToMove of
            White _ -> player == Player1
            Black _ -> player == Player2
            Empty   -> False
--
takeDestination :: Position -> StateT Game IO Position
takeDestination pos1 = do
    pos2  <- lift takeInput
    board <- gets getBoard
    let pieceToMove = getSquares board Map.! pos1
    if pos1 /= pos2 && checkMovement pieceToMove board pos1 pos2
        then return pos2
        else do
            lift . putStrLn $ "Invalid Move! Please choose another square to move your piece " ++ show pieceToMove
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
    Just (h, w)   <- lift getTerminalSize
    board <- gets getBoard
    config <- lift graphicsConfig
    lift $ runReaderT (printGrid . Map.keys . getSquares $ board) config
    lift $ runReaderT (printPieces board) config
    lift $ runReaderT  printBorder config
    lift $ setCursorPosition (h-5) 0
--
printBorder :: ReaderT GraphicsConfig IO ()
printBorder = do
    (sh, sw)     <- asks squareSize
    (bh, bw)     <- asks boardSize
    (vPos, hPos) <- asks boardPosition
    let hBorder = concatMap (: replicate (sw-1) ' ') ['a' .. 'h']
    lift $ setCursorPosition (vPos + 1) (hPos + div sw 2)
    lift $ putStr hBorder
    lift $ setCursorPosition (vPos - bh - 2) (hPos + div sw 2)
    lift $ putStr hBorder
    let vBorder = concatMap (: replicate (sh-1) ' ') ['1' .. '8'] `zip` [0..]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos - 2)      >> putStr [c] | (c, i) <- vBorder]
    sequence_ [lift $ setCursorPosition (vPos - div (sh+1) 2 - i) (hPos + 1 + bw) >> putStr [c] | (c, i) <- vBorder]
--
printGrid :: [Position] -> ReaderT GraphicsConfig IO ()
printGrid ks = do
    (sh, sw)     <- asks squareSize
    (vPos, hPos) <- asks boardPosition
    let evenSquares = [(vPos - y*sh , hPos + (x-1)*sw) | (x, y) <- ks, odd y /= odd x]
    c <- asks whiteChar
    let printSquare (x, y) = sequence_ [setCursorPosition (x + n) y >> putStr (replicate sw c) | n <- [0 .. sh-1]]
    lift $ mapM_ printSquare evenSquares
--
printPieces :: Board -> ReaderT GraphicsConfig IO ()
printPieces (Board squares) = do
    (sh, sw)     <- asks squareSize
    (vPos, hPos) <- asks boardPosition
    let printPiece (x, y) piece = setCursorPosition (vPos - y*sh + div sh 2) (hPos + (x-1)*sw + div sw 2) >> print piece
    lift $ sequence_ . Map.mapWithKey printPiece $ squares
--
----------------------------------------------------------------------------------------------------------------- |