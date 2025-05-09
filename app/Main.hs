import Control.Monad.State (runStateT)
import GameController (playGame)
import GameInit (newGame)
import UI.Config (graphicsConfig)
import UI.Terminal (welcomeScreen, zeroCursor)

main :: IO ()
main = do
    config <- graphicsConfig <$> welcomeScreen
    runStateT (playGame config) newGame
    zeroCursor
