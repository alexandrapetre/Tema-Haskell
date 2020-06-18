module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import ClassState
import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

-- Definire Program
type Program = Map String (String, ClassState)

initEmptyProgram :: Program
initEmptyProgram = Map.singleton "Global" ("Global", initEmptyClass)

getVars :: Program -> [[String]]
getVars myMapProg = removeDuplicates( getVars2 (Map.elems myMapProg))
getVars2 [] = []
getVars2 ((numParinte, classS):sir) = (getValues classS Var) ++ (getVars2 sir)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

getClasses :: Program -> [String]
getClasses myMapProg = Map.keys myMapProg

getParentClass :: String -> Program -> String
getParentClass strg myMapProg = getParent2 myMapProg strg (Map.keys myMapProg)
getParent2 myMapProg key (k1:ks) = if (k1 == key) 
								   then (if ((fst $ (myMapProg Map.! key)) == []) 
								   		then "Global" else (fst $ (myMapProg Map.! key)) ) 
								   else getParent2 myMapProg key ks


getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass strg myMapProg = getFuncs2 myMapProg strg (Map.keys myMapProg)
getFuncs2 myMapProg key [] = []
getFuncs2 myMapProg key (k1:ks) = if (k1 == key) then (getValues (snd  $ (myMapProg Map.! key)) Func) ++ (getFuncs2 myMapProg key ks) else (getFuncs2 myMapProg key ks)

-- Instruction poate fi ce considerați voi
type  Instruction = [String]

funct1 :: String -> [String]
funct1 strg = removeSpaces (lines strg)

funct2 :: [String] -> [Instruction]
funct2 [] = []
funct2 (line:sir) = (funct3 line) : (funct2 sir)

funct3 :: String -> [String]
funct3 s = removeSpaces (wordsAux s [] [])

wordsAux [] crt acc = acc ++ [crt]
wordsAux (x:xs) crt acc 
                        | x == ' ' = wordsAux xs [] (acc ++ [crt])
                        | x == '=' = wordsAux xs [] (acc ++ [crt])
                        | x == ':' = wordsAux xs [] (acc ++ [crt])
                        | x == '(' = wordsAux xs [] (acc ++ [crt])
                        | x == ')' = wordsAux xs [] (acc ++ [crt])
                        | x == ',' = wordsAux xs [] (acc ++ [crt])
                        | x == '.' = wordsAux xs [] (acc ++ [crt])
                        |otherwise = wordsAux xs (crt ++ [x]) acc

removeSpaces :: [String] ->[String]
removeSpaces [] = []
removeSpaces (x:xs) = if (x == "") then removeSpaces xs else x : (removeSpaces xs)

parse :: String -> [Instruction]
parse strg = funct2 (funct1 strg)
 
interpret :: Instruction -> Program -> Program
interpret (x:instr) myMapProg
	| instr == [] = myMapProg
	| x == "class" = initClass instr myMapProg
	| x == "newvar" = insertElem instr myMapProg
	| otherwise = insertFunc (x:instr) myMapProg

initClass :: Instruction -> Program -> Program
initClass instr myMapProg 
	|(length instr == 1) = (Map.insert (head instr) ("Global", initEmptyClass) myMapProg)
	|(length instr == 3) = initClass2 instr myMapProg (Map.keys myMapProg)
	|otherwise = myMapProg

initClass2 :: Instruction -> Program -> [String] -> Program
initClass2 instr myMapProg [] = Map.insert (head instr) ("Global", initEmptyClass) myMapProg
initClass2 instr myMapProg (k:ks) 
	| (k == (instr !! 2)) = Map.insert (head instr) (instr !! 2, initEmptyClass) myMapProg
	| otherwise = initClass2 instr myMapProg ks

insertElem :: Instruction -> Program -> Program
insertElem instr myMapProg
	| length instr < 2 = myMapProg
	| (verifClass instr (Map.keys myMapProg) /= True) = myMapProg
	| otherwise = Map.insert "Global" ("Global", (insertIntoClass (snd $ (myMapProg Map.! "Global")) Var instr)) myMapProg

verifClass :: Instruction -> [String] -> Bool
verifClass instr [] = False
verifClass instr (k1:ks) 
	| (instr !! 1 == k1) =  True
	| otherwise =  verifClass instr ks

insertFunc :: Instruction -> Program -> Program
insertFunc instr myMapProg
	| (verifClass instr (Map.keys myMapProg) /= True) = myMapProg
	| length instr >= 3 = interpretFuncs myMapProg instr
	| otherwise = myMapProg

interpretFuncs :: Program -> Instruction -> Program
interpretFuncs myMapProg instr
	| (verifValidElements (drop 1 (modif2 instr myMapProg)) myMapProg  == True) = Map.insert (instr !! 1) ((getParentClass (instr !! 1) myMapProg), (insertIntoClass (snd $ (myMapProg Map.! (instr !! 1))) Func (modif2 instr myMapProg))) myMapProg
	| otherwise = myMapProg

verifValidElements :: [String] -> Program -> Bool
verifValidElements [] myMapProg = True
verifValidElements (el:l) myMapProg = if (cautaClasa el (Map.keys myMapProg) /= True) then False else verifValidElements l myMapProg


cautaClasa :: String -> [String] -> Bool
cautaClasa cuv [] = False
cautaClasa cuv (k:ks) 
	| k == cuv = True
	|otherwise = cautaClasa cuv ks

verifClassReturn :: Instruction -> [String] -> Bool
verifClassReturn instr [] = False
verifClassReturn instr (k1:ks) 
	| (instr !! 0 == k1) =  True
	| otherwise =  verifClassReturn instr ks

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs = let elemI = xs !! i
                            elemJ = xs !! j
                            left = take i xs
                            middle = take (j - i - 1) (drop (i + 1) xs)
                            right = drop (j + 1) xs
                    	in  left ++ [elemJ] ++ middle ++ [elemI] ++ right

modif :: Instruction -> Program -> Instruction
modif instr myMapProg  = tail (swapElementsAt 0 1 (rmv 2 instr))

modif2 :: Instruction -> Program -> Instruction
modif2 instr myMapProg = swapElementsAt 0 1 (rmv 2 instr) 

rmv :: Int -> Instruction -> Instruction
rmv 0 [] = []
rmv inx instr = fst parts ++ snd parts
    			where parts = (take (inx-1) instr, drop inx instr)

infer :: Expr -> Program -> Maybe String
infer (Va strg) myMapProg
	| (existVar (getVars myMapProg) strg == True) = Just (getVar (getVars myMapProg) strg)
	| otherwise = Nothing

infer (FCall stg1 stg2 list) myMapProg
								| (existVar (getVars myMapProg) stg1 /= True) = Nothing
								| ((existFuc (getFuncsForClass (getVar (getVars myMapProg) stg1) myMapProg) stg2) /= True ) = Nothing
								| (verifVars list myMapProg /= True) = Nothing
								| otherwise = Just (typeFunc (getFuncsForClass (getVar (getVars myMapProg) stg1) myMapProg) stg2)

existFuc :: [[String]] -> String -> Bool
existFuc [] strg = False
existFuc (x:xs) strg
					| x !! 0 == strg = True
					| otherwise = existFuc xs strg

verifVars :: [Expr] -> Program -> Bool
verifVars [] myMapProg = True
verifVars (x:xs) myMapProg
		| (infer x myMapProg == Nothing) = False
		| otherwise = verifVars xs myMapProg

typeFunc :: [[String]] -> String -> String
typeFunc (x:xs) strg
					| x !! 0 == strg = x !! 1
					| otherwise = typeFunc xs strg


existVar :: [[String]] -> String -> Bool
existVar [] strg = False
existVar ([var,tip]:xs) strg
	| var == strg = True
	| otherwise = existVar xs strg

getVar :: [[String]] -> String -> String
getVar [] strg = ""
getVar ([var,tip]:xs) strg 
	| var == strg =  tip
	| otherwise = getVar xs strg
