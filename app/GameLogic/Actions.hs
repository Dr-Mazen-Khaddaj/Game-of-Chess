module GameLogic.Actions
    ( changePlayer
    , updateSkippedSquare
    , makeMove
    , updateKilledPieces
    , prepareEnPassantMove
    ) where

import Control.Monad (void)
import Control.Monad.State (MonadState, gets, modify)
import Types
    ( Game
        ( currentPlayer
        , getBoard
        , killedBlackPieces
        , killedWhitePieces
        , movedPieces
        , opponent
        , skippedSquare
        )
    , Piece (Pawn)
    , Player (Player1, Player2)
    , Position
    , Skipped (None, Only)
    , Square (..)
    )

import qualified Data.Map as Map

{- FOURMOLU_DISABLE -}

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
prepareEnPassantMove :: MonadState Game m => m ()
prepareEnPassantMove = do
    skippedSquare <- gets skippedSquare
    case skippedSquare of
        Only (posEmpty, posPawn) -> void $ makeMove posPawn posEmpty
        None -> error "Can't prepare En passant move!"
