module GameController (playGame) where

import Control.Monad (join, when)
import Control.Monad.Reader (ReaderT (..), liftIO)
import Control.Monad.State (MonadIO, MonadState, StateT, gets, lift, modify)
import Data.Char (toLower)
import Data.Map qualified as Map
import System.IO (hFlush, stdout)

import GameLogic.Actions
import GameLogic.Rules (checkGameOver, checkPromotion, kingNotInCheck)
import Types
import UI.Terminal

{- playGame -}
playGame :: GraphicsConfig -> StateT Game IO ()
playGame config = do
    let renderBoard = join $ gets (\game -> lift $ runReaderT (printBoard game) config)
    {- IO actions -}
    renderBoard
    playTurn
    renderBoard
    promotion <- checkPromotion
    when promotion (doPromotion >> renderBoard)
    changePlayer
    gameOver <- checkGameOver
    if gameOver
        then endGame config
        else playGame config

--
endGame :: GraphicsConfig -> StateT Game IO ()
endGame config = do
    player <- gets currentPlayer
    board <- gets getBoard
    if kingNotInCheck player board
        then lift $ runReaderT printStalemate config
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
doPromotion :: (MonadIO m, MonadState Game m) => m ()
doPromotion = do
    {- Variables -}
    player <- gets currentPlayer
    board <- gets getBoard
    let options = [Queen, Rook, Bishop, Knight]
    -- OPTIMIZE : revise logic, get rid of singleton pattern matching ([pawnPosition])
    let [pawnPosition] = case player of
            Player1 -> Map.keys $ Map.filterWithKey (\(_, y) s -> y == 8 && s == White Pawn) board
            Player2 -> Map.keys $ Map.filterWithKey (\(_, y) s -> y == 1 && s == Black Pawn) board
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
    modify $ \game -> game{getBoard = updatedBoard}

--
getPromotion :: (MonadIO m) => m Piece
getPromotion = do
    write " > "
    liftIO $ hFlush stdout
    answer <- liftIO getLine
    case map toLower answer of
        "queen" -> return Queen
        "rook" -> return Rook
        "bishop" -> return Bishop
        "knight" -> return Knight
        _ -> write "Invalid Response!\n" >> getPromotion
