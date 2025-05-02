import System.Random (randomRIO)
import Control.Monad (replicateM)

type WordList = [(String, Int)]  -- (slovo, počet slabik)

fiveSyllables :: WordList
fiveSyllables =
  [ ("Silent moonlight", 5), ("Winds embrace the hills", 5)
  , ("Frozen fields sleep", 5), ("Autumn leaves drift", 5)
  , ("Stars beyond the clouds", 5)
  ]

sevenSyllables :: WordList
sevenSyllables =
  [ ("Soft rain whispers on the roof", 7)
  , ("Cherry blossoms gently fall", 7)
  , ("Dreams awaken under trees", 7)
  , ("Lanterns glow in distant mist", 7)
  , ("Owls cry beneath pale branches", 7)
  ]

randomFrom :: WordList -> IO String
randomFrom wl = do
  index <- randomRIO (0, length wl - 1)
  return (fst (wl !! index))

generateHaiku :: IO String
generateHaiku = do
  l1 <- randomFrom fiveSyllables
  l2 <- randomFrom sevenSyllables
  l3 <- randomFrom fiveSyllables
  return $ unlines [l1, l2, l3]

main :: IO ()
main = do
  putStrLn "Generated Haiku:"
  putStrLn "----------------"
  poem <- generateHaiku
  putStrLn poem
