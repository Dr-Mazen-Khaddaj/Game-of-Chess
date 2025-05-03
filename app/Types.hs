module Types
    ( Piece (..)
    , Square (..)
    , Player (..)
    , Position
    , Board
    , Skipped (..)
    , Game (..)
    , CastlingInfo (..)
    , GraphicsConfig (..)
    )
where

{- FOURMOLU_DISABLE -}

import qualified Data.Map as Map

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
    show Pawn   = "Pawn"
    show Knight = "Knight"
    show Bishop = "Bishop"
    show Rook   = "Rook"
    show Queen  = "Queen"
    show King   = "King"
--
{- ♔ ♕ ♖ ♗ ♘ ♙ ♚ ♛ ♜ ♝ ♞ ♟ -}
instance Show Square where
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
    show player = case player of
        Player1 -> "Player 1"
        Player2 -> "Player 2"
