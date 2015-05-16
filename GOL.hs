--Just some imports
import System.IO
import Data.Char
import Control.Monad
import Control.Applicative
import qualified Data.Map as M

--Type synonyms
type CoOrd = (Int, Int)
type Cell  = Char --Either '#' for live or '-' for dead
type Grid  = M.Map CoOrd Cell

--Global varibles
gS = (3, 3) -- (rows, columns)
rules = ([2,3],[3]) --(Survival,Birth): Conway's Game of Life
onChar  = '#'
offChar = '-'

--Coordinate tuples
coords :: [CoOrd]
coords = (\r c -> (r,c)) <$> [1..(fst gS)] <*> [1..(snd gS)]

--Empty grid
emptyGrid :: Grid
emptyGrid = M.fromList . zip coords $ repeat offChar

--Converts grid to list of cells
gridToCells :: Grid -> [Cell]
gridToCells = M.elems

--Converts list of cells to grid
cellsToGrid :: [Cell] -> Grid
cellsToGrid = M.fromList . zip coords . filter isPrint

--Returns all surounding cells of a coordinate
surCells :: CoOrd -> Grid -> [Cell]
surCells co g = map (g M.!) (surCo (fst co) (snd co))
    where inR (r,c) = r `elem` [1..(fst gS)] && c `elem` [1..(snd gS)]
          surCo r c = filter inR
                      [(r+0, c+1), (r+1, c+1), (r+1, c+0), (r+1, c-1)
                      ,(r+0, c-1), (r-1, c-1), (r-1, c+0), (r-1, c+1)]

--Updates grid based on a set of rules
nextGen :: Grid -> Grid
nextGen g = cellsToGrid $ next coords g
    where surOn c g = length . filter (==onChar) $ surCells c g
          next :: [CoOrd] -> Grid -> String
          next [] _     = ""
          next (c:cs) g
              |surOn c g `elem` (snd rules) = [onChar] ++ (next cs g) --Birth
              |surOn c g `elem` (fst rules) = [g M.! c] ++ (next cs g) --Survival
              |otherwise                    = [offChar] ++ (next cs g) --Death

--Prints a list of cells
printCells :: [Cell] -> IO ()
printCells [] = putStr ""
printCells g  = putStrLn (take (snd gS) g) >> printCells (drop (snd gS) g)

--Make flexible command line interface:
--Take starting config from text file (default: use random starting grid)
--Automatically set grid size based off of file (default: 100x100)
--Set minimum delay between generations in milliseconds (use -d tag)
--Set alternitive rules (use -r tag)
main :: IO()
main = undefined
