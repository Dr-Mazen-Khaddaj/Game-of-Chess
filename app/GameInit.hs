module GameInit (newBoard, castlingInfo, newGame) where

import Types

import qualified Data.Map as Map

{- FOURMOLU_DISABLE -}

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
