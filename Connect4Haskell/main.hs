-- Adrian Gomez Rodriguez and Stephanie Medina.

import Data.List
import Data.Maybe
import System.IO
import System.Random
import Board

-- Main function to play.
play = do
    let bd = bdX 7 6
    let p = playerX
    putStrLn "\nConnect four By Adrian Gomez and Stephanie Medina\n"
    choice <- pickGame
    putStrLn (bdToStr bd playerToChar)
    putStrLn ""

    if(choice == 1)
    then do strategyGame bd p False
    else do game bd p

-- Play until player wins or enters -1 (quit)
game :: [[Int]] -> Int -> IO()
game bd p = do
    -- read input from col
    col <- readSlot bd p
    if not(col == (-1))
    then do
        let updatedBd = dropTocken bd col p
        let drawnBd = (bdToStr updatedBd playerToChar)
        if(isWon updatedBd p)
        then do
            putStrLn drawnBd
            putStr "Player with "
            printWinner p
            putStrLn " Beat you!"
        else do
            putStrLn drawnBd
            putStrLn ""
            game updatedBd (changePlayer p)
    else do
        putStrLn "\n Thank you, Bye Bye!!\n"

-- Strategy game vs computer.
strategyGame :: [[Int]] -> Int -> Bool -> IO()
strategyGame bd p compTurn = do
    if compTurn
    -- drop a tocken in a random value between 0 and 6
    then do
        col <- getRandomSlot
        let updatedBd = dropTocken bd col p
        let drawnBd = (bdToStr updatedBd playerToChar)
        if(isWon updatedBd p)
        then do
            putStrLn drawnBd
            putStr "Player "
            printWinner p
            putStrLn " beat you!!"
        else do
            putStrLn drawnBd
            putStrLn ""
            strategyGame updatedBd (changePlayer p) (changeTurn compTurn)
    else do
        col <- readSlot bd p
        if not(col == (-1))
        then do
            let updatedBd = dropTocken bd col p
            let drawnBd = (bdToStr updatedBd playerToChar)
            if(isWon updatedBd p)
            then do
                putStrLn drawnBd
                putStr "Player "
                printWinner p
                putStrLn " Beat you!"
            else do
                putStrLn drawnBd
                putStrLn ""
                strategyGame updatedBd (changePlayer p) (changeTurn compTurn)
        else do
            putStrLn "\n Thank you Bye Byeeeee!\n"

-- random number between 0 and 6
getRandomSlot :: IO(Int)
getRandomSlot = do
    num <- randomRIO(0,6) :: IO Int
    return num

changeTurn :: Bool -> Bool
changeTurn current
    | current = False
    | otherwise = True

-- Get index of availible slot and the function reads inputs
-- and returns an IO value such as IO(Int) or IO(Integer).
readSlot :: [[Int]] -> Int -> IO(Int)
readSlot bd p = do
    putStrLn ("Player " ++ playerToChar p ++ "'s turn")
    putStr ("Choose a number from 0 to 6 to drop tocken or type -1 to quit: ")
    line <- getLine
    let input = reads line :: [(Int, String)] in
      if length input == 0
      then readSlot'
      else let (col, _) = head input in
        if (col <= numOfSlot bd) &&
           (col >= 0 && col <= 6 )
        then return col
        else do
            if (col == (-1))
            then end
            else do readSlot'
    where
      readSlot' = do
        putStrLn "Invalid number, try again"
        readSlot bd p
      end = do
        return (-1)

-- select to play vs friend or vs computer.
pickGame :: IO(Int)
pickGame = do
    putStrLn "Do you want to play vs.."
    putStrLn "1 = VS Computer"
    putStrLn "2 = VS Friend"
    line <- getLine
    let input = reads line :: [(Int, String)] in
      if length input == 0
      then pickGame'
      else let (choice, _) = head input in
        if choice == 1 || choice == 2
        then return choice
        else do
            pickGame'
    where
      pickGame' = do
        putStrLn "Enter 1 or 2."
        pickGame

-- prints the player as winner
printWinner :: Int -> IO()
printWinner player = do
    if player == 1
    then putStr "1 (O)"
    else putStr "2 (X)"

-- print the board
playerToChar :: Int -> String
playerToChar player
 | (player == 1) = "O "
 | (player == 2) = "X "
 | otherwise = "- "
