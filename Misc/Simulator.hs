module Misc.Simulator where
import Statistics.ANOVA
import Data.Random
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Beta
import Data.Random.Distribution.Uniform
import Data.List
import qualified Data.Vector as V
import Control.Concurrent.ParallelIO

data Variation = Variation { 
                   name :: Int
                  ,trial :: Int
                  ,success :: Int 
                  ,trueRate :: Double 
                  ,status :: Status 
                   } deriving (Show, Eq)
                   

-- Int is the iteration on which the Variation was paused
data Status = Active | Paused Int deriving (Eq, Show)

data Action = PauseAndAdd Variation
              | DoNothing
              | Add  deriving (Show)
                
-- need IO to allow randomness
type SelectionMethod = [Variation] -> IO Variation                

data Strategy = Strategy {
  selection :: SelectionMethod
  ,action :: [Variation] -> IO Action
  }
                
runStrategy :: Int -> Strategy -> IO [Variation]                
runStrategy 0 strategy = do
  var1 <- newVariation []
  var2 <- newVariation [var1]
  return [var2, var1]
runStrategy n strategy = do   
  --get state for before nth iteration
  vars <- runStrategy (n-1) strategy
  --send in the visitor
  updatevars <- newVisitors 1000 (selection strategy) vars
  --determine action
  a <- (action strategy) updatevars
  --take action
  takeAction a updatevars

incrementVariation :: Variation -> IO Variation
incrementVariation v = do
  res <- sample (bernoulli (trueRate v))
  return $ v{trial = 1 + trial v, success = res + success v}

randomSelection :: SelectionMethod
randomSelection vs = runRVar (randomElement active) StdRandom
  where active = filter (\x -> status x == Active) vs
                
takeAction :: Action -> [Variation] -> IO [Variation]
takeAction DoNothing vs = return vs
takeAction (PauseAndAdd x) vs = do
  let iter = sum $ map trial vs
      pausedList = replaceListElement x (x{status=Paused iter}) vs 
  newVar <- newVariation pausedList
  return $ newVar : pausedList
takeAction Add vs = do
  v <- newVariation vs                  
  return $ v:vs


newVisitor :: SelectionMethod -> [Variation] -> IO [Variation]
newVisitor selection variations = do
  var <- selection variations
  newVar <- incrementVariation var       
  return $ replaceListElement var newVar variations
  
newVisitors :: Int -> SelectionMethod -> [Variation] -> IO [Variation]
newVisitors 0 _ v = return v
newVisitors n method vars = do
  v <- newVisitors (n-1) method vars
  newVisitor method v
  
  
--how to do conversion rate for this?  
--just do random for now  
newVariation :: [Variation] -> IO Variation
newVariation [] = do
  convr <- sample $ beta 1.1 150
  return $ Variation 1 0 0 convr Active
newVariation vs = do
  convr <- sample $ beta 5 150 --arbitrary
  return $ Variation ((+) 1 $ maximum $ map name vs) 0 0 convr Active

--replaces ALL occurences
replaceListElement old new [] = []
replaceListElement old new (x:xs)
  | old == x = new:(replaceListElement old new xs)
  | otherwise = x:(replaceListElement old new xs)             
                
pickNew :: Strategy
pickNew = Strategy randomSelection (\x -> return $ PauseAndAdd $ last $ filter (\x -> status x == Active) x)

pickRandom :: Strategy
pickRandom = Strategy randomSelection (\x -> fmap PauseAndAdd (randomSelection x))

pickBest = Strategy randomSelection (\x -> return $ PauseAndAdd $ worst x)
  where worst vars = fst $ head $ sortBy (\x y -> compare (snd x) (snd y)) $
                     map (\var -> (var, (fromIntegral $ success var)/(fromIntegral $ trial var))) $ 
                     filter (\x -> status x == Active) vars

doAnova :: Double -> Strategy
doAnova pvalue = Strategy randomSelection (return . fisherLSDAction pvalue)

cheat :: Strategy
cheat = Strategy randomSelection (\x -> return $ PauseAndAdd $ worst x)
  where worst vars = fst $ head $ sortBy (\x y -> compare (snd x) (snd y)) $
                     map (\var -> (var, trueRate var)) $ 
                     filter (\x -> status x == Active) vars

transformVariations :: [Variation] -> V.Vector (Int, Double)
transformVariations [] = V.fromList [] 
transformVariations (v:vs) = (V.fromList $ replicate (success v) (name v,1)) V.++ (V.fromList $ replicate (trial v - success v) (name v, 0)) V.++ (transformVariations vs)

fisherLSDAction pvalue variations  
  | (==) 0 $ sum $ map success $ active variations = DoNothing
  | otherwise = case groups of
    [] -> DoNothing
    x:[] -> DoNothing
    grouping -> PauseAndAdd worst
  where groups = fisherLSD pvalue $ transformVariations $ active variations
        worst = fst $ head $ sortBy (\x y -> compare (snd x) (snd y)) $ map (\var -> (var , (fromIntegral $ success var) / (fromIntegral $ trial var))) $ active variations
        trials = sum $ map trial $ active variations
        active = filter (\v -> status v ==Active) 
        
epsilonGreedy :: SelectionMethod
epsilonGreedy variations = do
  v <- runRVar doubleStdUniform StdRandom
  if v < 0.1 then randomSelection variations
             else return $ chooseBest variations    
  where chooseBest vars =  fst $ last $ sortBy (\x y -> compare (snd x) (snd y)) $
                           map (\var -> (var, (fromIntegral $ success var)/(fromIntegral $ trial var))) $ 
                           filter (\x -> status x == Active) vars
  