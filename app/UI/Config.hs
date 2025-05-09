module UI.Config (graphicsConfig) where

import Types (GraphicsConfig (..))

-- Graphics Configuration --
graphicsConfig :: ((Int, Int), Char) -> GraphicsConfig
graphicsConfig ((h, w), whiteChar) = GraphicsConfig (h, w) boardSize squareSize boardPosition playerBox landscapeOrientation whiteChar
    where
        thirdOfWidth = div w 3
        isAlmostSquare = h > thirdOfWidth && h < 2 * thirdOfWidth
        landscapeOrientation = h < div w 2
        s
            | landscapeOrientation =
                if isAlmostSquare
                    then h `div` 12
                    else h `div` 9
            | isAlmostSquare = w `div` 28
            | otherwise = w `div` 23
        {- Square Size -}
        squareSize@(sh, sw) = (s, 2 * s + 1)
        {- Board Size -}
        boardSize@(bh, bw) = (sh * 8, sw * 8)
        {- Board Position -}
        (vShift, hShift)
            | landscapeOrientation = (div (h - bh) 2, div (w - bw) 2)
            | otherwise = (h - bh - 8, div (w - bw) 2)
        boardPosition = (h - vShift, hShift)
        {- Player Box -}
        playerBoxWidth
            | landscapeOrientation = min 22 (hShift - 10)
            | otherwise = min 22 bw
        playerBoxSpace
            | landscapeOrientation = max 0 (playerBoxWidth - 10) `div` 4
            | otherwise = max 0 $ min 2 (div bw 15 - 2)
        playerBox = (5 + 2 * playerBoxSpace, playerBoxSpace)
