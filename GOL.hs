--Just some imports:
import System.IO --For reading .txt files
import Data.Char --For filtering strings
import System.Random --For generating random staring configurations
import Control.Monad --For "piping" IO
import System.Environment --For saving terminal args
import Control.Applicative --The fancy <$> and <*> functions
import qualified Data.Map as M --For storing our grid

--Notes:
--Finish IO

--Type synonyms
type CoOrd = (Int, Int)
type Cell  = Char --Either '#' for live or '-' for dead
type Grid  = M.Map CoOrd Cell

--Global constants
gS = (40, 100) -- (rows, columns)
rules = ([2,3],[3]) --(Survival,Birth): Conway's Game of Life
onChar  = '#' --"Live" cell
offChar = '-' --"Dead" cell

--Coordinate tuples
coords :: [CoOrd]
coords = (\r c -> (r,c)) <$> [1..(fst gS)] <*> [1..(snd gS)]

--Converts grid to list of cells
gridToCells :: Grid -> [Cell]
gridToCells = M.elems

--Converts list of cells to grid
cellsToGrid :: [Cell] -> Grid
cellsToGrid = M.fromList . zip coords . filter isPrint

--Returns all surounding cells of a coordinate
surCells :: CoOrd -> Grid -> [Cell]
surCells co g = map (g M.!) (surCo co) -- "Looks up" all of the surrounding cells
    where rw x = if (x<1) then x+(fst gS) else if (x>(fst gS)) then x-(fst gS) else x --Wraps rows
          cw x = if (x<1) then x+(snd gS) else if (x>(snd gS)) then x-(snd gS) else x --Wraps columns
          surCo (r,c) = [(rw (r+0), cw (c+1)), (rw (r+1), cw (c+1))
                        ,(rw (r+1), cw (c+0)), (rw (r+1), cw (c-1))
                        ,(rw (r+0), cw (c-1)), (rw (r-1), cw (c-1))
                        ,(rw (r-1), cw (c+0)), (rw (r-1), cw (c+1))]

--Updates grid based on a set of rules
nextGen :: Grid -> Grid
nextGen g = cellsToGrid $ next coords g
    where surOn c g = length . filter (==onChar) $ surCells c g --Returns the number of "live" neighbors
          next [] _     = ""
          next (c:cs) g
              |surOn c g `elem` (snd rules) = [onChar] ++ (next cs g) --Birth
              |surOn c g `elem` (fst rules) = [g M.! c] ++ (next cs g) --Survival
              |otherwise                    = [offChar] ++ (next cs g) --Death

--Prints a list of cells
printCells :: [Cell] -> IO ()
printCells [] = putStrLn ""
printCells g  = putStrLn (take (snd gS) g) >> printCells (drop (snd gS) g)

--Make flexible command line interface:
--Take starting config from text file (default: use random starting grid)
--Automatically set grid size based off of file (default: 100x100)
--Set minimum delay between generations in milliseconds (use -d tag)
--Set alternitive rules (use -r tag)
main :: IO()
main = do
    args <- getArgs --Stores command line arguments
    g <- getStdGen --Stores "seed" for the random generator
    if (args == []) --If no arguements are given then... otherwise...
        then simulate $ cellsToGrid . map (\x -> if x then onChar else offChar) $ randomRs (True,False) g
        else readFile (head args) >>= simulate . cellsToGrid . filter isPrint
        where simulate g = do --Prints current grid, updates the grid, then repeats
                        printCells . gridToCells $ g
                        simulate $ nextGen g
