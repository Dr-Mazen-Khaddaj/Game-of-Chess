module UI.Terminal
    ( welcomeScreen
    , zeroCursor
    , printBoard
    , printStalemate
    , printCheckmate
    , takePieceToMove
    , takeDestination
    , write
    )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.State (StateT, gets, lift)
import Data.List (intercalate, intersperse)
import Data.Map qualified as Map
import System.Console.ANSI
    ( clearScreen
    , cursorBackward
    , cursorDown
    , cursorUp
    , getTerminalSize
    , setCursorColumn
    , setCursorPosition
    )
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import GameLogic.Actions (prepareEnPassantMove)
import GameLogic.Rules
    ( checkCastleMove
    , checkEnPassantMove
    , checkKingAfterMove
    , checkMovement
    , kingNotInCheck
    , possibleMovements
    )
import Types
    ( Board
    , Game
        ( currentPlayer
        , getBoard
        , killedBlackPieces
        , killedWhitePieces
        , opponent
        )
    , GraphicsConfig
        ( boardPosition
        , boardSize
        , landscapeOrientation
        , playerBox
        , squareSize
        , whiteChar
        )
    , Player (..)
    , Position
    , Square (..)
    )
import UI.Config (graphicsConfig)

{- FOURMOLU_DISABLE -}

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
