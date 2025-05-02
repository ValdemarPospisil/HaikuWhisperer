import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (intercalate)

-- Datové typy pro nálady a témata
data Mood = Happy | Sad | Reflective deriving (Show, Enum, Bounded)
data Theme = Rain | Forest | Night | City deriving (Show, Enum, Bounded)

-- Pomocná funkce pro výběr náhodného prvku
randomFrom :: [a] -> IO a
randomFrom xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)

-- Výběr z předem definovaných možností
promptChoice :: (Show a, Enum a, Bounded a) => String -> IO a
promptChoice label = do
  putStrLn $ "\n" ++ label
  let options = [minBound .. maxBound]
  mapM_ (\(i, opt) -> putStrLn $ show i ++ ". " ++ show opt) (zip [1..] options)
  putStr "Zadej číslo: "
  idx <- readLn
  return (options !! (idx - 1))

-- Slovníky podle nálady a tématu
getWordLists :: Mood -> Theme -> ([String], [String], [String])
getWordLists Happy Forest =
  ( ["Morning sun breaks through", "Laughing leaves dancing"]
  , ["Squirrels leap on mossy trunks", "Birdsongs echo far and wide"]
  , ["Joy hides in green shade", "Peace returns with wind"]
  )
getWordLists Sad Night =
  ( ["Empty streets at dusk", "Cold wind sighs alone"]
  , ["Darkness swallows silent stars", "Footsteps fade into the void"]
  , ["Nothing left to say", "Tears fall with the moon"]
  )
getWordLists Reflective Rain =
  ( ["Drops paint the window", "Stillness in my breath"]
  , ["Thoughts like rivers slowly pass", "Puddles mirror what I am"]
  , ["Grey clouds drift within", "Past lives drip away"]
  )
getWordLists _ _ =
  ( ["Leaves fall quietly", "Silent moonlight shines"]
  , ["Dreams awaken under trees", "Lanterns glow in distant mist"]
  , ["Peace comes after rain", "Stars begin to blink"]
  )

-- Generování haiku z dat
generateHaiku :: Mood -> Theme -> IO String
generateHaiku mood theme = do
  let (l1s, l2s, l3s) = getWordLists mood theme
  l1 <- randomFrom l1s
  l2 <- randomFrom l2s
  l3 <- randomFrom l3s
  return $ intercalate "\n" [l1, l2, l3]

-- Hlavní funkce
main :: IO ()
main = do
  putStrLn "Vítej v generátoru Haiku!"
  mood <- promptChoice "Vyber náladu:"
  theme <- promptChoice "Vyber téma:"
  putStrLn "\nVygenerované Haiku:"
  putStrLn "--------------------"
  poem <- generateHaiku mood theme
  putStrLn poem
