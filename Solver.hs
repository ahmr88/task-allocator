module Solver where

import Types
import qualified Data.Vector as V
import Data.Maybe
import Control.Applicative
import Data.List
import Data.Function

type TaskDep = [(Task, [Task])]

type AllocTable = [(String, V.Vector Alloc)]

onSnd :: (a, b) -> (b -> c) -> (a, c)
onSnd (x,y) = (,) x . (flip ($)) y

lastEnd :: [String] -> AllocTable -> Int
lastEnd mmbrs tbl = maximum $ map (findEnd tbl) mmbrs
  where
    findEnd tbl mmbr = case lookup mmbr tbl of
      Nothing -> 0
      Just as -> end $ V.head as


mergeDict :: Eq a => [(a,b)] -> [(a,b)] -> [(a,b)]
mergeDict new orig = foldl' keepNew new orig
  where 
    keepNew acc (key, tsks) = case lookup key acc of
      Nothing -> (key, tsks) : acc
      Just xs' -> acc


allocate :: Task -> AllocTable -> AllocTable
allocate t@(Task n dur mmbrs) tbl = mergeDict addedTasks tbl
  where 
    addedTasks = map (flip onSnd $ V.cons allocation) filteredAllocs
    allocation = Alloc t availStart (availStart + dur)
    availStart = lastEnd mmbrs tbl
    filteredAllocs = map (\mm -> case lookup mm tbl of 
                            Nothing -> (mm, V.empty)
                            Just xs -> (mm, xs)
                         ) mmbrs

isAllocated :: Task -> AllocTable -> Bool
isAllocated t@(Task name _ _) at = case lookup name at of
                              Nothing -> False
                              Just xs -> V.elem t $ V.map task xs

deadTime :: AllocTable -> [(String, Int)]
deadTime at = map (flip onSnd $ dead) at
  where
    done = lastEnd (map fst at) at
    dead allocs = 
      done - foldl (\sum -> (+) sum . throughput) 0 allocs
    throughput = time . task

schedCost :: AllocTable -> Int
schedCost = sum . map (snd) . deadTime

solver :: ([Tree Task], AllocTable) -> Maybe ([Tree Task], AllocTable)
solver (ts, at) =
  case filteredLeaves of
    [] -> Just (ts, at)
    xs  -> do 
      sols <- sequence $ map recurs xs
      return $ minimumBy (compare `on` schedCost . snd) sols
    where 
      recurs t = solver (map (removeLeaf t) ts, allocate t at)
      filteredLeaves = filter (not . (flip isAllocated) at) 
                     $ concatMap leaves ts

solveSched :: [(Task, [Task])] -> Maybe ([Tree Task], AllocTable)
solveSched deps = do
  trees <- depsToTrees deps
  solver (trees, [])

alloc :: Task -> Int -> Alloc
alloc t strt = Alloc t strt $ strt + time t

testAllocTable :: AllocTable
testAllocTable = [ ("Amir", V.singleton 
                          $ alloc (Task "code" 5 ["Amir"]) 0)
                 , ("John", V.singleton
                          $ alloc (Task "plan" 3 ["John"]) 0)
                 ]

testTask = Task "review2" 6 ["Jack"]
testTask2 = Task "review" 4 ["Amir", "John", "Jack"]

taskDeps = [(testTask, [testTask2]), (testTask2, [])]
