module ClassState
where

import Data.Map (Map)
import qualified Data.Map as Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func  deriving (Show, Eq)
data ClassState = Cons (Map [String] InstrType) deriving Show

-- TODO - Trebuie definit ClassState

initEmptyClass :: ClassState
initEmptyClass = (Cons Map.empty)

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (Cons myMap) elem strg = (Cons (Map.insert strg elem myMap))

getValues :: ClassState -> InstrType -> [[String]]
getValues (Cons myMap) instruction  = Map.keys $ (Map.filter (== instruction) myMap)
