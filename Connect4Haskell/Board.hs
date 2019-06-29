-- Adrian Gomez Rodriguez and Stephanie Medina.

module Board(bdX, playerX, opponent, comp, dropTocken, isEmpty, numOfSlot, height, isFull, isWon, bdToStr, changePlayer) where

import Data.List
import Data.List.Split
import Data.Array
import Data.Maybe

--  Create a m by n board
bdX :: Int -> Int -> [[Int]]
bdX m n = (chunksOf n listOfZeros)
    -- Create a List of size (m * n) filled with zeros
    where listOfZeros = (take (m * n) (repeat 0))

-- Players
playerX :: Int
playerX = 1
opponent :: Int
opponent = 2
comp :: Int
comp = 3

-- Changes to the opposite player
changePlayer :: Int -> Int
changePlayer player | player == 1 = opponent | otherwise = playerX

-- Drop tocken in availible position and return a new board
dropTocken :: [[Int]] -> Int -> Int -> [[Int]]
dropTocken bd index player = replaceCol bd index newCol
    where oldCol = getCol bd index;
          emptySpotIndex = getEmptySpot oldCol;
          newCol = replaceAt emptySpotIndex player oldCol

--  Finds the next empty place
getEmptySpot col = last (findIndices(==0) col)
getCol :: [[Int]] -> Int -> [Int]
getCol bd colIndex = bd !! colIndex

-- Update the col
replaceCol :: [[Int]] -> Int -> [Int] -> [[Int]]
replaceCol bd oldIndex newCol = firstPart ++ [newCol] ++ lastPart
    where firstPart = take (oldIndex) bd;
          lastPart = drop (oldIndex + 1) bd

--  Replaces - with the value
replaceAt index value (x:xs)
    | index == 0 = value:xs
    | otherwise = x:replaceAt (index-1) value xs

--  If the availible plase has a 0 then replace token
isEmpty :: [[Int]] -> Int -> Bool
isEmpty bd index = elem 0 col
    where col = bd !! index

--  Returns the # of col of a board bd.
numOfSlot :: [[Int]] -> Int
numOfSlot bd = length bd

--  Returns the # of row of a board bd
height :: [[Int]] -> Int
height bd
    | (length bd == 0) = 0
    | otherwise = length (bd !! 0)

--  check number of empty places at board
getTotal :: [[Int]] -> Int
getTotal bd = (numOfSlot bd) * (height bd)

-- Check if the board is not full
isFull :: [[Int]] -> Bool
isFull bd = not (elem 0 (concat bd))

-- check if player won
isWon :: [[Int]] -> Int -> Bool
isWon bd player = isWonByRecursive bd player 0

--  Is the game played on a board board won by a specified player?
isWonByRecursive :: [[Int]] -> Int -> Int -> Bool
isWonByRecursive bd player spot
    | spot == (getTotal bd) = False
    | isWinAt bd player spot = True
    | otherwise = isWonByRecursive bd player (spot+1)

--  Check where the player won.
isWinAt :: [[Int]] -> Int -> Int -> Bool
isWinAt bd player index
    -- The player didn't win!
    | outOfBounds bd col row = False
    -- Check the current spot
    | (not (getTokenAt bd col row == player)) = False
    | (isWon == True) = True
    -- Check the next spot in the board
    | otherwise = isWinAt bd player (index + 1)
    where col = indexToCol bd index;
          row = indexToRow bd index;

    -- Count the number of tokens in each direction.
          rightCount = count bd col row 1 0 player 4;
          leftCount = count bd col row (-1) 0 player 4;
          upCount = count bd col row 0 1 player 4;
          downCount = count bd col row 0 (-1) player 4;
          leftDiagUp = count bd col row (-1) 1 player 4;
          leftDiagDown = count bd col row (-1) (-1) player 4;
          rightDiagUp = count bd col row 1 1 player 4;
          rightDiagDown = count bd col row 1 (-1) player 4;
      -- Check if they have 3 tokens in any direction
          isWon = (leftCount == 3) || (rightCount == 3) ||
                  (upCount == 3) || (downCount == 3) ||
                  (leftDiagUp == 3) ||
                  (leftDiagDown == 3) ||
                  (rightDiagUp == 3) ||
                  (rightDiagDown == 3);

--  Count the tokens of the player
count bd col row colDir rowDir player maxCount
    | (maxCount == 1) = 0
    | (getTokenAt bd newCol newRow == player) = 1 + recursiveCall
    | otherwise = recursiveCall
    where newCol = (col + colDir);
          newRow = (row + rowDir)
          recursiveCall = count bd newCol newRow colDir rowDir player (maxCount - 1)

--  Return a string representation of a board bd.
bdToStr :: [[Int]] -> (Int -> String) -> String
bdToStr bd playerToChar = bdToStrRecursive bd playerToChar 0

--  converting a board bd to a string representation and keep track of current board position
bdToStrRecursive :: [[Int]] -> (Int -> String) -> Int -> String
bdToStrRecursive bd playerToChar index
    | (col == (totalCols - 1)) = tokenToStr ++ newLine ++ recursiveCall
    | (index == totalTokens) = ""
    | otherwise = tokenToStr ++ recursiveCall
    where recursiveCall = bdToStrRecursive bd playerToChar (index + 1);
          totalCols = numOfSlot bd;
          totalTokens = getTotal bd;
          isLastRow = index == (totalTokens - 1);
          newLine = if (isLastRow) then "" else "\n";
          col = indexToCol bd index;
          row = indexToRow bd index;
          token = getTokenAt bd col row
          tokenToStr = playerToChar token

--  Returns -1 if the location is out of bounds
getTokenAt :: [[Int]] -> Int -> Int -> Int
getTokenAt bd col row
    | outOfBounds bd col row = -1
    | otherwise = (bd !! col) !! row

-- Checks if the location in board bd is out of bounds
outOfBounds :: [[Int]] -> Int -> Int -> Bool
outOfBounds bd col row
    | col < 0 = True
    | row < 0 = True
    | col >= numOfSlot bd = True
    | row >= height bd = True
    | otherwise = False

-- Converts an index to a row/col number of board bd
indexToCol :: [[Int]] -> Int -> Int
indexToCol bd index = mod index (numOfSlot bd)
indexToRow :: [[Int]] -> Int -> Int
indexToRow bd index = quot index ((height bd) + 1)
