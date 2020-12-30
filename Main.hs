import           Control.Monad   (guard)
import           Data.List       (intercalate, sort)
import           Data.List.Split (splitOn)
import qualified Data.Map        as M

-- I assume that every csv has an header

type CSVRow = M.Map String String
type CSV = [CSVRow]


readCSV ::  FilePath -> IO CSV
readCSV fp = do
  xs <- fmap (splitOn ",") . lines <$> readFile fp
  let h = head xs
  let rows = tail xs
  pure $ M.fromList . zip h <$> rows

writeCSV :: FilePath -> CSV -> IO ()
writeCSV fp d = writeFile fp $ unlines (h:s)
  where
    toStrRow = intercalate ","
    s :: [String]
    s = toStrRow . fmap snd . M.toList <$> d
    h :: String
    h = toStrRow $ fst <$> M.toList (head d)

join :: String -> CSV -> CSV -> CSV
join k d1 d2 = do
  x <- d1
  y <- d2
  guard $ x M.! k == y M.! k
  pure $ M.union x y

pd1 :: FilePath
pd1 = "d1.csv"

pd2 :: FilePath
pd2 = "d2.csv"

pout :: FilePath
pout = "out.csv"

joinOn :: String
joinOn = "key"

main :: IO()
main = do
  d1 <- readCSV indicators
  d2 <- readCSV scores
  writeCSV pout (join key d1 d2)
