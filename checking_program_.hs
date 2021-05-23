{-compile with "ghc -Rghc-timing -O2 checking_program_v3.hs"-}

import System.Environment
import Data.List
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.Map.Strict as Map

type RegisteredPositions = IntMapS.IntMap [Int]

--A little help with triples
fstOfThree (a, _, _) = a
sndOfThree (_, b, _) = b
thrOfThree (_, _, c) = c

--And then some with quads
fstOfFour (a, _, _, _) = a
sndOfFour (_, b, _, _) = b
thrOfFour (_, _, c, _) = c

--This function is a single pass test for single digit factors
--It will be called as many times as needed by pryForSDFactors
trySingleDigitsFactors :: (Bool, Integer, [Integer]) -> (Bool, Integer, [Integer])
trySingleDigitsFactors (bool, number, []) = (bool, number, [])
trySingleDigitsFactors (bool, number, (factor:fs))
   | mod number factor == 0 = (True, div number factor, fs)
   | otherwise = trySingleDigitsFactors (False, number, fs)

--This function will take a number and repeatedly divide by single digits till it gets to a single digit if possible
--Then it will return True
pryForSDFactors :: Integer -> Bool
pryForSDFactors n
   | sndOfThree sdfTry < 10 = True
   | fstOfThree sdfTry == True = pryForSDFactors $ sndOfThree sdfTry
   | otherwise = False
   where sdfTry = trySingleDigitsFactors (False, n, [2,3,5,7])

toDigits :: Integer -> [Int]
toDigits n = map (\n -> read [n]) (show n)

fromDigits :: Integral a => [a] -> Integer
fromDigits = foldl shiftAndAdd 0
    where shiftAndAdd acc d = 10 * acc + fromIntegral d

replaceElementAtPos :: a -> Int -> [a] -> [a]
replaceElementAtPos newElement pos [] = []
replaceElementAtPos newElement 0 (x:xs) = newElement:xs
replaceElementAtPos newElement pos (x:xs) = x : replaceElementAtPos newElement (pos-1) xs

insertHelper :: [Int] -> [Int] -> [Int]
insertHelper (x:_) values = x:values

lookupHelper :: Maybe [a] -> [a]
lookupHelper Nothing = []
lookupHelper (Just x) = x

--shiftLAtIndex :: Int -> [Int] -> [Int]
--shiftLAtIndex 

shiftLByOne :: Int -> [Int] -> [Int]
shiftLByOne elem [] = elem:[]
shiftLByOne elem [x] = x:elem:[]
shiftLByOne elem (x:xs) = x:shiftLByOne elem xs

checkPermutationsStep :: ([Int], Int, [Int], RegisteredPositions) -> ([Int], Int, [Int], RegisteredPositions)
checkPermutationsStep (digits, index, rotationMap, registeredPositions)
   -- Rule 1: If you 're at the last digit index-wise, go back to the previous digit. Notice that testPermsWithRep (see until) has already checked for a successful exit condition, so if you ended up here again it means you have to keep searching
   | index == digitsLength - 1                                               = (digits, index-1, rotationMap, registeredPositions)
   -- Rule 2: If the digit has not been encountered before at this particular index go for it and move to the next index (go deeper)
   | not $ elem digitAtIndex $ lookupHelper $ IntMapS.lookup index registeredPositions = (digits, index+1, rotationMap, IntMapS.insertWith insertHelper index [digitAtIndex] registeredPositions)
   -- Rule 3: If all digits at this particular index have been checked take a step back index-wise and both clear registered Position beyond the current depth AND restore rotationMap for this index
   | rotationAtIndex == 0                                                    = (digits, index-1, restoredRotMap, restoredRegPositions)
   -- Rule 4: If not all digit on this index have been checked but this particular digit has been encountered before, skip it, shift the digit list at this point and subtract the rotation counter 
   | rotationAtIndex > 0 && (elem digitAtIndex $ lookupHelper $ IntMapS.lookup index registeredPositions) = (shiftLDigits, index, subtractRot, registeredPositions)
   where digitsLength = length digits
         digitAtIndex = head $ drop index digits
         rotationAtIndex = head $ drop index rotationMap
         restoredRotMap = replaceElementAtPos (digitsLength - index) index rotationMap
         restoredRegPositions = IntMapS.delete index registeredPositions  --clear everything below the parent index
         shiftLDigits = concat [fstOfSplitDigits, shiftLByOne (head sndOfSplitDigits) (tail sndOfSplitDigits)]
         (fstOfSplitDigits, sndOfSplitDigits) = splitAt index digits
         subtractRot = replaceElementAtPos (rotationDigitAtIndex - 1) index rotationMap
         rotationDigitAtIndex = head $ drop index rotationMap

checkConditions :: ([Int],Int,[Int],RegisteredPositions) -> Bool
checkConditions (digits, index, rotationMap, registeredPositions)
   | (index == 0 && rotationAtIndex == 0) || ((index == (length digits) - 1) && pryForSDFactors (fromDigits digits)) = True
   | otherwise = False
   where rotationAtIndex = head $ drop index rotationMap

testPermsWithRep :: Integer -> Integer
testPermsWithRep n
   | sndOfFour computationResult == 0 && (head . thrOfFour) computationResult == 0 = 0
   | otherwise = (fromDigits . fstOfFour) computationResult
   where computationResult = until checkConditions checkPermutationsStep (digitsOfn, 0 , [digitsLength, digitsLength -1 .. 1], IntMapS.empty)
         digitsOfn = toDigits n
         digitsLength = length digitsOfn

main :: IO ()
main = do
   args <- getArgs
   let inputNumber = read (head args) :: Integer
   let checkResult = testPermsWithRep inputNumber
   print checkResult
