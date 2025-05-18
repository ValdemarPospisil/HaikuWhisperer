import System.Random (randomRIO)
import Control.Monad (replicateM, when)
import Data.List (intercalate, sort)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Map as Map

-- Datové typy pro nálady a témata
data Mood = Happy | Sad | Reflective | Anxious | Nostalgic 
  deriving (Show, Enum, Bounded, Eq, Ord)

data Theme = Rain | Forest | Night | City | Ocean | Mountain | Desert 
  deriving (Show, Enum, Bounded, Eq, Ord)

-- Datová struktura pro ukládání historie
data HaikuHistory = HaikuHistory {
  poemText :: String,
  poemMood :: Mood,
  poemTheme :: Theme,
  timestamp :: String
} deriving (Show)

-- Globální historie básní
type HaikuHistoryMap = Map.Map Int HaikuHistory
haikuHistory :: HaikuHistoryMap
haikuHistory = Map.empty

-- Pomocná funkce pro výběr náhodného prvku
randomFrom :: [a] -> IO a
randomFrom xs = do
  idx <- randomRIO (0, length xs - 1)
  return (xs !! idx)

-- Vylepšená funkce pro výběr z možností s potvrzením
promptChoice :: (Show a, Enum a, Bounded a, Ord a) => String -> IO a
promptChoice label = do
  putStrLn $ "\n" ++ label
  let options = sort [minBound .. maxBound]
  mapM_ (\(i, opt) -> putStrLn $ show i ++ ". " ++ show opt) (zip [1..] options)
  putStr "Zadej číslo (nebo 0 pro náhodný výběr): "
  hFlush stdout  -- Zajistí, že se prompt okamžitě zobrazí
  idx <- readLn
  if idx == 0
    then randomFrom options
    else if idx >= 1 && idx <= length options
         then return (options !! (idx - 1))
         else do
           putStrLn "Neplatná volba, zkus to znovu."
           promptChoice label

-- Funkce pro ověření, zda haiku splňuje požadovaný počet slabik (5-7-5)
validateHaiku :: String -> Bool
validateHaiku poem = 
  let lines = words <$> lines poem
      counts = map length lines
  in  length counts == 3 && counts == [5, 7, 5]

-- Funkce pro uložení vygenerovaného haiku do historie
saveHaiku :: String -> Mood -> Theme -> IO Int
saveHaiku poem mood theme = do
  currentTime <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
      newHistory = HaikuHistory poem mood theme timeStr
      newId = Map.size haikuHistory + 1
  return newId

-- Funkce pro export haiku do souboru
exportHaiku :: String -> Mood -> Theme -> IO ()
exportHaiku poem mood theme = do
  currentTime <- getCurrentTime
  let timeStr = formatTime defaultTimeLocale "%Y%m%d%H%M%S" currentTime
      fileName = "haiku_" ++ show mood ++ "_" ++ show theme ++ "_" ++ timeStr ++ ".txt"
      folderPath = "output"
  
  -- Vytvoření složky, pokud neexistuje
  createDirectoryIfMissing True folderPath
  
  -- Uložení básně do souboru
  writeFile (folderPath ++ "/" ++ fileName) $ 
    "Haiku\n" ++
    "Mood: " ++ show mood ++ "\n" ++
    "Theme: " ++ show theme ++ "\n" ++
    "Date: " ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime ++ "\n\n" ++
    poem
  
  putStrLn $ "Haiku uloženo do souboru: " ++ fileName

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
-- Přidaná nová kombinace pro Anxious City
getWordLists Anxious City =
  ( ["Traffic roars like waves", "Ticking clocks too loud"]
  , ["Crowds swarm through concrete canyons", "Eyes avoid meeting strangers' gaze"]
  , ["Heart beats much too fast", "No peace in this place"]
  )
-- Přidaná nová kombinace pro Nostalgic Ocean
getWordLists Nostalgic Ocean =
  ( ["Childhood shores return", "Salt spray on my lips"]
  , ["Memories float on gentle tides", "Seashells whisper stories old"]
  , ["Time washed out to sea", "Waves bring past to shore"]
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

-- Generování sekvence haiku
generateHaikuSequence :: Int -> Mood -> Theme -> IO [String]
generateHaikuSequence count mood theme
  | count <= 0 = return []
  | otherwise = do
      sequence <- replicateM count (generateHaiku mood theme)
      return sequence

-- Zobrazení menu
showMenu :: IO ()
showMenu = do
  putStrLn "\nVyber akci:"
  putStrLn "1. Vygenerovat jedno haiku"
  putStrLn "2. Vygenerovat sekvenci haiku"
  putStrLn "3. Exportovat haiku do souboru"
  putStrLn "4. Konec"
  putStr "Tvá volba: "
  hFlush stdout

-- Hlavní funkce programu
main :: IO ()
main = do
  putStrLn "Vítej v generátoru Haiku 2.0!"
  mainLoop

mainLoop :: IO ()
mainLoop = do
  showMenu
  choice <- readLn
  case choice of
    1 -> do
      mood <- promptChoice "Vyber náladu:"
      theme <- promptChoice "Vyber téma:"
      putStrLn "\nVygenerované Haiku:"
      putStrLn "--------------------"
      poem <- generateHaiku mood theme
      putStrLn poem
      
      when (not $ validateHaiku poem) $
        putStrLn "Upozornění: Toto haiku nemusí dodržovat tradiční formát 5-7-5 slabik."
      
      -- Uložení do historie
      _ <- saveHaiku poem mood theme
      mainLoop
    
    2 -> do
      mood <- promptChoice "Vyber náladu:"
      theme <- promptChoice "Vyber téma:" 
      putStr "Kolik haiku chceš vygenerovat? "
      hFlush stdout
      count <- readLn
      putStrLn "\nVygenerovaná sekvence Haiku:"
      putStrLn "-------------------------"
      sequence <- generateHaikuSequence count mood theme
      mapM_ (\(i, p) -> do
          putStrLn $ "Haiku #" ++ show i ++ ":"
          putStrLn p
          putStrLn ""
        ) $ zip [1..] sequence
      mainLoop
    
    3 -> do
      mood <- promptChoice "Vyber náladu:"
      theme <- promptChoice "Vyber téma:"
      putStrLn "\nVygenerované Haiku k exportu:"
      putStrLn "----------------------------"
      poem <- generateHaiku mood theme
      putStrLn poem
      exportHaiku poem mood theme
      mainLoop
    
    4 -> putStrLn "Děkuji za použití generátoru Haiku. Nashledanou!"
    
    _ -> do
      putStrLn "Neplatná volba, zkus to znovu."
      mainLoop
