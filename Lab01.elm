module Lab01 exposing (tally, relativelyPrime, nestedLoopIndexes, hourglass)

module Main exposing (main) --task 1
import Html exposing (text)
import Dict exposing (Dict)
import Set exposing (Set)

characterCounts : String -> Dict String Int
characterCounts str =
    let
        characterList = String.split "" str -- convert input str to a list of characters
        uniqueCharacters = Set.fromList characterList -- get unique characters in characterList
        
        countCharacter : String -> (String, Int)
        countCharacter character =
            let
                isEqual : String -> Bool
                isEqual other =
                    character == other

                makeTuple : Int -> (String, Int)
                makeTuple count =
                    (character, count)
            in 
            List.filter isEqual characterList
            |> List.length
            |> makeTuple
    in
    
    List.map countCharacter (Set.toList uniqueCharacters)
    |> Dict.fromList

main =
    Html.text "" 


module Main exposing (..) --Task 2

import Html exposing (..)


relativelyPrime : Int -> Int -> Bool
relativelyPrime a b =
    greatestCommonDivisor a b == 1


greatestCommonDivisor : Int -> Int -> Int
greatestCommonDivisor a b =
    if b == 0 then
        a
    else
        greatestCommonDivisor b (remainderBy a b)


main =
    Html.text ""


import Html exposing (..) -- Task 3
import Array exposing (..) 
nestedLoopIndexes : (Int, Int) -> (Int, Int) -> List (Int, Int)
nestedLoopIndexes (startX, startY) (endX, endY) =
    let
        xIndexes =
            List.range start end

        yIndexes =
            if startY == endY then
                xIndexes
            else
                List.reverse xIndexes

        addValue : Int -> (Int, Int) -> (Int, Int)
        addValue index (secondStart, secondEnd) =
            if remainderBy 2 index == 0 then 
                (index, secondStart)
            else 
                (index, secondEnd)
    in
    List.map (addValue >> Tuple.pair endY) yIndexes


import Dict exposing (Dict) -- Task 4

hourglass : Int -> String
hourglass size =
    let
        edgeRow : Int -> String
        edgeRow x =
            "o" ++ String.repeat ((2 * x) - 1) "-" ++ "o"

        topBodyRows =
            List.range 1 size

        topBodyRowGen x =
            if x == 1 then
                edgeRow size

            else
                String.repeat (x - 1) " " ++ "\\" ++ String.repeat (2 * (size - (x - 1)) - 1) " " ++ "/"

        topBody =
            topBodyRows
                |> List.map topBodyRowGen
                |> String.join "\n"

        bottomBodyRows =
            List.range 1 size
                |> List.reverse

        bottomBodyRowGen x =
            if x == 1 then
                edgeRow size

            else
                String.repeat (x - 1) " " ++ "/" ++ String.repeat (2 * (size - (x - 1)) - 1) "." ++ "\\"

        bottomBody =
            bottomBodyRows
                |> List.map bottomBodyRowGen
                |> String.join "\n"

        middleRow =
            String.repeat size " " ++ "x"
    in
    topBody ++ "\n" ++ middleRow ++ "\n" ++ bottomBody 
