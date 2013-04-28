
module Main where

import Data.List
import Data.Char






-- |The core IO part of the code
main = 
	do
		dictionary <- readFile "src/linuxwords.txt"
		let list = (lines dictionary)

		putStrLn "1. Words containing string"
		putStrLn "2. Words starting with string"
		putStrLn "3. Words ending with string"
		putStrLn "4. Words that contain certain letters somewhere"
		putStrLn "5. Quit"

		strChoice <- getLine


		if(strChoice == "1")
			then do 

			putStrLn "Give me a string"
			putStrLn "Use '$' as a wildcard"
			str <- getLine

			let matches = map (containsWildcard str) list

			let hits = contains list matches



			putStrLn "\nI am looking for words...\n"
			putStrLn "1. LESS THAN a certain number of letters"
			putStrLn "2. GREATER THAN a certain number of letters"
			putStrLn "3. EQUAL to a certain number of letters"
			putStrLn "4. I don't care"

			option <- getLine

			if(option == "4")
				then
					putStrLn(unlines hits)
					else
						do
						putStrLn "# of letters?"
						strLength <- getLine

						if(checkNumber strLength == True)
							then do
							let length = read strLength :: Int

							if(option == "1")
								then
									putStrLn(unlines(lessThan hits length))

									else if(option == "2")
										then
										putStrLn(unlines(greaterThan hits length))

									else if(option == "3")
										then
										putStrLn(unlines(equalTo hits length))
									else
										putStrLn "Invalid Input \n"
								else
									putStrLn "Invalid Input \n"
			main


			else if(strChoice == "2")
				then do 

				putStrLn "Give me a string"
				putStrLn "Use '$' as a wildcard"
				str <- getLine

				let matches = map (startsWith str) list

				let hits = contains list matches

				putStrLn "\nI am looking for words...\n"
				putStrLn "1. LESS THAN a certain number of letters"
				putStrLn "2. GREATER THAN a certain number of letters"
				putStrLn "3. EQUAL to a certain number of letters"
				putStrLn "4. I don't care"

				option <- getLine

				if(option == "4")
					then
						putStrLn(unlines hits)
						else
							do
							putStrLn "# of letters?"
							strLength <- getLine

							if(checkNumber strLength == True)
								then do
								let length = read strLength :: Int

								if(option == "1")
									then
										putStrLn(unlines(lessThan hits length))

										else if(option == "2")
											then
											putStrLn(unlines(greaterThan hits length))

										else if(option == "3")
											then
											putStrLn(unlines(equalTo hits length))
										else
											putStrLn "Invalid Input \n"
									else
										putStrLn "Invalid Input \n"
				main


			else if(strChoice == "3")
				then do
				putStrLn "Give me a string"
				putStrLn "Use '$' as a wildcard"
				str <- getLine

				let matches = map (endsWith str) list

				let hits = contains list matches

				putStrLn "\nI am looking for words...\n"
				putStrLn "1. LESS THAN a certain number of letters"
				putStrLn "2. GREATER THAN a certain number of letters"
				putStrLn "3. EQUAL to a certain number of letters"
				putStrLn "4. I don't care"

				option <- getLine

				if(option == "4")
					then
						putStrLn(unlines hits)
						else
							do
							putStrLn "# of letters?"
							strLength <- getLine

							if(checkNumber strLength == True)
								then do
								let length = read strLength :: Int

								if(option == "1")
									then
										putStrLn(unlines(lessThan hits length))

										else if(option == "2")
											then
											putStrLn(unlines(greaterThan hits length))

										else if(option == "3")
											then
											putStrLn(unlines(equalTo hits length))
										else
											putStrLn "Invalid Input \n"
								else
									putStrLn "Invalid Input \n"
				main


			else if(strChoice == "4")
				then do
				putStrLn "Give me a string"
				str <- getLine


				let hits = containsElem list str

				putStrLn "\nI am looking for words...\n"
				putStrLn "1. LESS THAN a certain number of letters"
				putStrLn "2. GREATER THAN a certain number of letters"
				putStrLn "3. EQUAL to a certain number of letters"
				putStrLn "4. I don't care"

				option <- getLine

				if(option == "4")
					then
						putStrLn(unlines hits)
						else
							do
							putStrLn "# of letters?"
							strLength <- getLine

							if(checkNumber strLength == True)
								then do
								let length = read strLength :: Int

								if(option == "1")
									then
										putStrLn(unlines(lessThan hits length))

										else if(option == "2")
											then
											putStrLn(unlines(greaterThan hits length))

										else if(option == "3")
											then
											putStrLn(unlines(equalTo hits length))
										else
											putStrLn "Invalid Input \n"
								else
									putStrLn "Invalid Input \n"
				main
			else if(strChoice == "5")
				then
				putStrLn "Goodbye! \n"
			else
				do
				putStrLn "Invalid Choice \n"
				main
	
	
-- |Takes a list of words, and a list of Booleans
-- and outputs the words corresponding to the the True
-- values in the list of Booleans
--
-- To be used with 'startsWith', 'endsWith', and 'containsWildcard' functions

contains :: [String] -> [Bool] -> [String]
contains (x:xs) [] = []
contains [] (y:ys) = []
contains [] [] = []
contains (x:xs) (y:ys)  
	|(y) = [x] ++ contains xs ys
	|otherwise = [] ++ contains xs ys
	
	
	
-- |Takes in a list of words and a list of characters, and outputs a subset of the list of words which contain all of the input characters

containsElem :: [String] -> [Char] -> [String]
containsElem [] y = []
containsElem x [] = x
containsElem x (y:ys) = containsElem (contains x (map (elem y) x)) ys

-- |Outputs a subset of strings which have lengths less than the input Int, from the input [String] 

lessThan :: [String] -> Int -> [String]
lessThan [] y = []
lessThan (x:xs) y
	|((length x) < y) = [x] ++ lessThan xs y
	|otherwise = [] ++ lessThan xs y
	
-- |Outputs a subset of strings which have lengths greater than the input Int, from the input [String] 

greaterThan :: [String] -> Int -> [String]
greaterThan [] y = []
greaterThan (x:xs) y
	|((length x) > y) = [x] ++ greaterThan xs y
	|otherwise = [] ++ greaterThan xs y



-- |Outputs a subset of strings which have lengths equal to the input Int, from the input [String] 

equalTo :: [String] -> Int -> [String]
equalTo [] y = []
equalTo (x:xs) y
	|((length x) == y) = [x] ++ equalTo xs y
	|otherwise = [] ++ equalTo xs y
	
	
	
-- | Checks if input String is a Digit
--
-- Used for user input selection

checkNumber :: [Char] -> Bool
checkNumber (x:xs)
	|(length xs) == 0 = isDigit x
	|(length xs) > 0 = isDigit x && checkNumber xs
	|otherwise = False
checkNumber [] = False



-- | Checks if a string starts with another string
--
-- Also utilizes wildcards denoted by \"$\" in the second argument

startsWith :: [Char]->[Char] -> Bool
startsWith [] _ = True
startsWith _ [] = False
startsWith (x:xs) (y:ys)
	|x == '$' = startsWith xs ys
	|otherwise = x == y && startsWith xs ys
	


-- | Checks if a string contains a list of characters
--
-- Also utilizes wildcards denoted by \"$\" in the second argument

containsWildcard :: [Char] -> [Char] -> Bool
containsWildcard x y = any (startsWith x) (tails y)



-- | Checks if a string ends with another string
--
-- Also utilizes wildcards denoted by \"$\" in the second argument

endsWith :: [Char] -> [Char] -> Bool
endsWith x y = reverse x `startsWith` reverse y

		
	

	





	
	


