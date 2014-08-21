import Numeric.Probability.Distribution
import Control.Monad
import Text.Printf

data Gene  = E2 | E3 | E4
   deriving (Eq, Ord, Show, Enum)
type GenePair = (Gene,Gene)
type GeneProbability = T Double Gene
type GenePairProbability = T Double GenePair
type YesProbability = T Double Bool

-- http://en.wikipedia.org/wiki/Apolipoprotein_E#Polymorphisms 
eurpeanDist :: GeneProbability
eurpeanDist = fromFreqs [(E2,0.10),(E3, 0.77),(E4, 0.13)]

larryDist :: GeneProbability
larryDist = fromFreqs [(E3, 0.5),(E4, 0.5)]

normalProbability:: YesProbability
normalProbability= getYesProbability =<< genes
    where
        genes = combine eurpeanDist eurpeanDist

adamProbability :: YesProbability
adamProbability = getYesProbability =<< genes 
    where 
          genes = combine father mother
          father = larryDist
          mother = eurpeanDist

-- https://www.23andme.com/you/journal/alzheimers/overview/
getYesProbability :: GenePair -> YesProbability
getYesProbability (x,y) = case (x,y) of
    (E2,E2) -> chooseBool (2 / 100)
    (E3,E2) -> chooseBool (3 / 100)
    (E4,E2) -> chooseBool (3 / 100)
    (E3,E3) -> chooseBool (7 / 100)
    (E4,E3) -> chooseBool (14 / 100)
    (E4,E4) -> chooseBool (70 / 100)

chooseBool :: Double ->  YesProbability
chooseBool p = choose p True False

stringValue :: YesProbability -> String
stringValue = printf "%.2f" . (* 100). truth

combine ::   GeneProbability -> GeneProbability ->  GenePairProbability
combine f m = norm $ liftM2 (,) f m
    where norm = cons . fmap normalize . decons

-- Order doesn't matter, so order pairs so that they will be combined
normalize :: (GenePair, Double) -> (GenePair, Double)
normalize ((a1,a2),b) | a1 > a2 = ((a1,a2), b)
                      | a1 <= a2 = ((a2,a1), b)
main :: IO ()
main = do
        putStrLn "Odds of getting Alzheimer's"
        putStrLn $ "Children Of Lary:   \t " ++ (stringValue adamProbability)
        putStrLn $ "Average:\t " ++ (stringValue normalProbability)
