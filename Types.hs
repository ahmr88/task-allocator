module Types where

import Data.Function
import Data.List


data Task = Task { name     :: String
                 , time     :: Int
                 , members  :: [String]
                 } deriving (Show)

instance Eq Task where
  (==) = (==) `on` name

data Alloc = Alloc { task   :: Task
                   , start  :: Int
                   , end    :: Int
                   } deriving (Show)

data Tree a = Nil | Node a [Tree a] deriving (Show, Eq)

instance Functor Tree where
  fmap f Nil = Nil
  fmap f (Node t deps) = Node (f t) (map (fmap f) deps)

isEmpty :: Tree a -> Bool
isEmpty Nil = True
isEmpty _ = False

isLeaf :: Tree a -> Bool
isLeaf (Node _ []) = True
isLeaf _ = False

leaves :: Tree a -> [a]
leaves Nil = []
leaves n@(Node a deps)
  | isLeaf n = [a]
  | otherwise = concatMap leaves deps

removeLeaf :: Eq a => a -> Tree a -> Tree a
removeLeaf a Nil = Nil
removeLeaf a (Node t []) = if a == t then Nil else (Node t [])
removeLeaf a (Node t xs) = Node t 
                         $ filter (not . isEmpty) 
                         $ map (removeLeaf a) xs

has :: Eq a => a -> Tree a -> Bool
has _ Nil = False
has x (Node n deps) = (n == x) || any (has x) deps

findRoots :: Eq a => [(a, [a])] -> [a]
findRoots deps = filter isRoot $ map fst deps
  where allDeps = concatMap snd deps
        isRoot = not . (flip elem) allDeps

rootToTree :: Eq a => [(a, [a])] -> [a] -> a -> Maybe (Tree a)
rootToTree ds prevTs t = case elem t prevTs of 
  True -> Nothing
  False -> case lookup t ds of
    Just tds -> Node t <$> recurs tds
    Nothing -> Just $ Node t []
  where
    recurs = sequence . map (rootToTree ds (t:prevTs))

depsToTrees :: Eq a => [(a, [a])] -> Maybe [Tree a]
depsToTrees deps = sequence $ map (rootToTree deps []) $ findRoots deps


testTree :: Tree Int
testTree = Node 0 [Node 1 [], Node 2 [], Node 3 []]

testDeps :: [(Int, [Int])]
testDeps = [(2, [1]), (1, [5,3]), (4, [1])]
